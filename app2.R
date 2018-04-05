# library and globals ---------------------

library(shiny)
library(shinydashboard)
library(ggplot2)

# globals -------------------------------------



mySystem <- list(
  dz1 = function(t,zs) (T/m)*((((x^2+y^2)^.5+((L-x)^2+(h-y)^2)^.5)/l0)-1)*
    ((L-x)/((L-x)^2+(h-y)^2)^.5)-(x/(x^2+y^2)^.5),
  dz2 = function(t,zs) 9.81 + (T/m)*((((x^2+y^2)^.5+((L-x)^2+(h-y)^2)^.5)/l0)-1)*
    ((h-y)/(((x0-x)^2+(h-y)^2)^.5)-(y/(x^2+y^2)^.5)),
  dz3 = function(t,zs) zs[1],
  dz4 = function(t,zs) zs[2]
)

limit <- 100000
rungeKutta <- function(t0,z0,L,stepSize,fns){
  n <- stepSize
  t <- numeric(limit)
  size <- length(fns)
  results <- matrix(0,nrow=limit+1,ncol=size+1)
  results[1,] <- c(t0,z0)
  
  counter <- 0
  stuck <- FALSE
  targetReached <- FALSE
  overlimit <- FALSE
  i <-2
  while(!stuck & !targetReached & counter <= limit){
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
      k1[m] <- n*fns[[m]](t_old,z_old)
      }
    #stage 2 loop
    for(m in 1:size){
      k2[m] <- n*fns[[m]](t_old+n/2, z_old+k1/2)
      }
    #stage 3 loop
    for(m in 1:size){
      k3[m] <- n*fns[[m]](t_old+n/2, z_old+k2/2)
      }
    #stage 4 loop
    for(m in 1:size){
      k4[m] <- n*fns[[m]](t_old+n, z_old+k3)
      }
    
    k <- (1/6)*(k1+2*k2+2*k3+k4)
    
    results [i,1] <- old[1]+n
    results[i,-1] <- z_old+k 
    
    if (results [i,2] < results[i-1,2]){
      stuck <- TRUE
    }
    if (results [i,2] >= L){
      targetReached <- TRUE
    }
    i <- i+1
    
  }
  if (counter > limit){
    overlimit <- TRUE
  }
  colnames(results) <- c("t",paste0("z",1:size))
  list(df = as.data.frame(results)[1:(counter+1),],
       stuck = stuck,
       targetReached = targetReached,
       overlimit = overlimit)
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
                       numericInput("h", value = 100, label="zip tower height"),
                       numericInput("n", value = .01, min =.000001, max = 1, 
                                    step = .001, label="step size"),
                       numericInput("L", value = 50, label = "distance between points"),
                       numericInput("m", value = 150, label= "your weight"),
                       numericInput("l0", value = 150, label = "unstretched wire length"),
                       actionButton("compute",label = "Compute!",
                                    icon = icon("calculator"))
                ),
                column(width = 9,
                       dataTableOutput("xyvalues"),
                       br(),
                       htmlOutput("report")
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
    position = NULL,
    system = NULL,
    stuck = FALSE,
    targetReached = FALSE,
    overlimit = FALSE
  )
  # Also must fix this
  observeEvent(input$compute, {
    res$system <- list(
      dz1 = function(t, zs) .58591,
      dz2 = function(t, zs) {
        t + 2 * zs[1] + input$h + input$L
      },
      dz3 = function(t,zs) zs[1],
      dz4 = function(t,zs) zs[2]
    )
    # input$y0 is where I need to input long function
    #h goes straight into des not rk
    
    T <- (2.10*(10^11))*0.1016
    L <- input$L
    h <- input$h
    l0 <- input$l0
    m <- input$m
    
    yFunc <- function(y0) ((y0+((L^2+(h-y0)^2)^.5)/l0)-1)*(1-((h-y0)/(L^2+(h-y0)^2)^.5))-((m*9.81)/T)
    
    y0 <- uniroot(yFunc, c(-1,2))$root
    
    rk <- rungeKutta(t0 = 0, z0= c(0,0,0,y0), L = input$L,
                     stepSize = input$n, fns = res$system)
    res$position <- data.frame(xf = rk$df$system)
    res$stuck <- rk$stuck
    res$targerReached <- rk$targetReached
    res$overLimit <- rk$overlimit
  })
  output$plot <- renderPlot({
    ggplot(data = res$position, mapping = aes(x = x, y = y)) +
      geom_line()
  })
  
  output$xyvalues <- renderDataTable({
    res$poition
  }, options = list(pageLength = 10,
                    lengthMenu = c(5,10,15)))
  output$report <- renderText({
    print(res$stuck)
    print(res$overLimit)
    if (res$stuck){
      return(HTML("<p><strong>You conquered the zipline!</strong></p>"))
    }
    if (res$overLimit){
      return(HTML(paste0("<p><strong>Target too far away to compute!",
                         "Maybe try a bigger stepsize.</strong></p>")))
    }
  })
}

# Run the App -------------------------------------

shinyApp(ui, server)
