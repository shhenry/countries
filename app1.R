# library and globals ---------------------

library(shiny)
library(plotly)
# library(ggplot2)

# globals ------------------------

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
  rungeKutta <- function(x, y, x1, n, f) {
    h <- (x1 - x) / n
    X <- numeric(n + 1)
    Y <- numeric(n + 1)
    X[1] <- x
    Y[1] <- y
    for ( i in 1:n ) {
      xhalf <- x + 0.5 * h
      xnew <- x + h
      k1 <- f(x, y)
      u1 <- y + 0.5 * h *k1
      k2 <- f(xhalf, u1)
      u2 <- y + 0.5 * h * k2
      k3 <- f(xhalf, u2)
      u3 <- y + h * k3
      k4 <- f(xnew, u3)
      k <- (k1 + 2 * k2 + 2 * k3 + k4) / 6
      x <- xnew
      y <- y + k * h
      X[i + 1] <- x
      Y[i + 1] <- y
    }
    data.frame(x = X, y = Y)
  }
  
  # this is the f(x,y) in the DE:  dy/dx = f(x, y)
  myDEFunc <- function(x, y) {
    y
  }
  # Tab 1:  Popular Languages ----------------------------  
  
  
  
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
