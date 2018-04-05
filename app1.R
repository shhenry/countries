# library and globals ---------------------

library(shiny)
library(shinydashboard)
library(ggplot2)

# globals -------------------------------------

mySystem <- list(
  dz1 = function(t,zs) .58591,
  dz2 = function(t,zs) -0.35156,
  dz3 = function(t,zs) zs[1],
  dz4 = function(t,zs) zs[2]
)

# no upper limit on time, if x cor is smaller than the one before theyre stuck

rungeKutta <- function(t0,z0,xf,stepSize,fns){
  n <- stepSize
  t <- numeric(100000)
  size <- length(fns)
  results <- matrix(0,nrow=100000+1,ncol=size+1)
  results[1,] <- c(t0,z0)
  
  counter <- 0
  stuck <- FALSE
  targetReached <- FALSE
  i <-2
  while(!stuck & !targetReached & counter < 100000){
    counter <- counter+1
  
    old <- results [i-1,]
    t_old <- old[1]
    z_old <- old [-1]
    k1 <- numeric(size)
    k2 <- numeric(size)
    k3 <- numeric(size)
    k4 <- numeric(size)
    
    #stage 1 loop 
    for(m in 1:size){
      k1[m] <- n*fns[[m]](t_old,z_old)}
    #stage 2 loop
    for(m in 1:size){
      k2[m] <- n*fns[[m]](t_old+n/2, z_old+k1/2)}
    #stage 3 loop
    for(m in 1:size){
      k3[m] <- n*fns[[m]](t_old+n/2, z_old+k2/2)}
    #stage 4 loop
    for(m in 1:size){
      k4[m] <- n*fns[[m]](t_old+n, z_old+k3)}
    
    k <- (1/6)*(k1+2*k2+2*k3+k4)
    
    results [i,1] <- old[1]+n
    results[i,-1] <- z_old+k 
    
     if (results [i,2] < results[i-1,2]){
      stuck <- TRUE
    }
    if (results [i,2] >= xf){
      targetReached <- TRUE
    }
    i <- i+1

  }
  
  colnames(results) <- c("t",paste0("z",1:size))
  as.data.frame(results)[1:(counter+1), ]
}

# ui ------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Zipline"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "runge", icon = icon("globe")),
      menuItem("Graph", tabName = "plot", icon = icon("line-chart")),
      HTML(paste0('<li><a href="report.pdf" target="_blank">',
                  '<i class="fa fa-file-pdf-o" aria-hidden="true"></i> Documentation</a></li>')),
      HTML(paste0('<li><a href="https://github.com/shhenry/zipline" target="_blank">',
                  '<i class="fa fa-github" aria-hidden="true"></i> Source Code</a></li>'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "runge",
              fluidRow(
                column(width = 3,
                       helpText("Enter inputs."),
                       numericInput("y0", value = 100, label="zip tower height"),
                       numericInput("n", value = .01, min =.000001, max = 1, 
                                    step = .001, label="step size"),
                       numericInput("xf", value = 50, label = "distance between points")
                ),
                column(width = 9,
                       dataTableOutput("xyvalues")
                )
              )
      ),
      tabItem(tabName = "plot",
              fluidRow(
                plotOutput("plot")
                )
              )
    )
  )
)


# server ----------------------------------------

server <- function(input, output, session) {

#--------------------------------------------------#
  
  res <- reactiveValues(
    position = NULL
  )
  
  output$tblLanguages <- renderText({
    input$region
  })
  output$plot <- renderPlot({
    ggplot(data = res$position, mapping = aes(x = x, y = y)) +
      geom_line()
  })

  output$xyvalues <- renderDataTable({
    rk <- rungeKutta(t0 = 0, z0 = c(0,0,0,input$y0), xf = input$xf,  
               stepSize = ceiling(120/(input$n)), fns = mySystem)
    res$position <- data.frame(x = rk$z3, y = rk$z4)
    rk
  }, options = list(pageLength = 10,
                    lengthMenu = c(5,10,15)))
}

# Run the App -------------------------------------

shinyApp(ui, server)
