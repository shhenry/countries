# library and globals ---------------------

library(shiny)

# library(ggplot2)


# globals ------------------------



# ui ------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "World Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Popular Languages", tabName = "languages1", icon = icon("globe")),
      HTML(paste0('<li><a href="report.pdf" target="_blank">',
                  '<i class="fa fa-file-pdf-o" aria-hidden="true"></i> Documentation</a></li>')),
      HTML(paste0('<li><a href="https://github.com/shhenry/zipline" target="_blank">',
                  '<i class="fa fa-github" aria-hidden="true"></i> Source Code</a></li>'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "languages1",
              fluidRow(
                column(width = 3,
                       helpText("Languages of the world are ranked according to the total number ",
                                "of people who speak them.  If you like you may restrict to ",
                                "countries in a particular region of the world."),
                  selectInput("region", label="Restrict to a Region:",
                              choices=c(letters))
                ),
                column(width = 9,
                  textOutput("tblLanguages")
                )
              )
      )
    )
  )
)

# server ----------------------------------------

server <- function(input, output, session) {

# reactive values ----------------------------------------------- 
  # tab1 = Popular Languages, tab2 = language/country, tab3 = country/languages
  # tab4 = langProsperity, tab5 = otherPairs
  # resp gives response variable (GNPpc or Life expectancy) in langProsperity
  #rv <- reactiveValues(tab1 = NULL)
  


  
# Tab 1:  Popular Languages ----------------------------  
    
  
  
  output$tblLanguages <- renderText({
    input$region
  })

}

# Run the App -------------------------------------

shinyApp(ui, server)
