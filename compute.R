# library and globals ---------------------

library(shiny)
library(plotly)

# ui ------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Zipline"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Runge-Kutta Play", tabName = "runge", icon = icon("globe")),
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
                       numericInput("x0", value = 0, label="starting x-value"),
                       numericInput("y0", value = 1, label="starting y-value"),
                       numericInput("x1", value = 1,label="ending x-value"),
                       numericInput("n", value = 100, min =1, step = 1, label="number of steps")
                ),
                column(width = 9,
                       dataTableOutput("xyvalues")
                )
              )
      )
    )
  )
)


# server ----------------------------------------

server <- function(input, output, session) {
  # x is xo, y is y0, x1 is x-value for which you want y, n is number of steps,
  # f is the function f in the DE dy/dx = f(f,)
  # do thalf and tnew instead of x
  #four functions f1 f2 f3 f4, each block of rk will have to run each f 
  initz <- c(5,10,0,10)
  mySystem <- list(
    dz1 = function(t,zs) zs[2],
    dz2 = function(t,zs) zs[1]^2+zs[3]+2*zs[4]+2*t,
    dz3 = function(t,zs) zs[4],
    dz4 = function(t,zs) zs[1]+zs[2]+3*zs[3]^2+5*t
  )
  
  rungeKutta <- function(t0,z0,t1,steps,fns){
    h <- (t1-t0)/steps
    t <- numeric(steps+1)
    size <- length(fns)
    results <- matrix(0,nrow=steps+1,ncol=size+1)
    results[1,] <- c(t0,z0)
    
    for(i in 2:(steps+1)){
      old <- results [i-1,]
      args <- list(t_old=old[1], z_old=old[-1])
      k1 <- numeric(size)
      k2 <- numeric(size)
      k3 <- numeric(size)
      k4 <- numeric(size)
      
      #stage 1 loop 
      for(m in 1:size){
        k1[m] <- h*fns[[m]](args$t_old,args$z_old)}
      #stage 2 loop
      for(m in 1:size){
        k2[m] <- h*fns[[m]](args$t_old*h/2, args$z_old+k1/2)}
      #stage 3 loop
      for(m in 1:size){
        k3[m] <- h*fns[[m]](args$t_old*h/2, args$z_old+k2/2)}
      #stage 4 loop
      for(m in 1:size){
        k4[m] <- h*fns[[m]](args$t_old*h, args$z_old+k3)}
      
      k <- (1/6)*(k1+2*k2+2*k3+k4)
      
      results [i,1] <- old[1]+h
      results[i,-1] <- k
    }
    
    colnames(results) <- c("t",paste0("z",1:size))}
    
    data.frame(x = X, y = Y)
  }
  # 45/n+1 assume max length of the ride to determine array size. trunkate the array when x gets to some 
  # value while x does not exceed the user input, keep calculating 
  # this is the f(x,y) in the DE:  dy/dx = f(x, y)
  # xp(t) = Vx(t), x(0)=0
  # yp(t) = Vy(t), y(0)=6
  # Vxp(t) = k1, Vx(0)=0
  # five columns of output not two
  myDEFunc <- function(z1,z2,z3,z4,t) {
    (z2,.58591,z4,-.035156)
    #(z2,f(z1,z2,z3,z4,t),z4,g(z1,z2,z3,z4,t))
  }
  
  #--------------------------------------------------#
  
  output$tblLanguages <- renderText({
    input$region
  })
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  output$xyvalues <- renderDataTable({
    rungeKutta(x = input$x0, y = input$y0, x1 = input$x1, n = input$n, f = myDEFunc)
  })
}

# Run the App -------------------------------------

shinyApp(ui, server)
