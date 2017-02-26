# library and globals ---------------------

library(shiny)
library(shinydashboard)
library(ggplot2)


# globals ------------------------

# for a selectInput
regions <- c("Caribbean","Southern and Central Asia", "Central Africa",           
             "Southern Europe","Middle East","South America",          
             "Polynesia","Antarctica", "Australia and New Zealand",
             "Western Europe","Eastern Africa","Western Africa",         
             "Eastern Europe","Central America","North America",            
             "Southeast Asia", "Southern Africa","Eastern Asia",             
             "Nordic Countries","Northern Africa","Baltic Countries",         
             "Melanesia","Micronesia","British Islands",          
             "Micronesia/Caribbean")  

# fake data for mock tables
var1 <- 1:50
var2 <- rep("filler", 50)
var3 <- rep("more filler", 50)
tab <- data.frame("First Varible"= var1, Second=var2, Third=var3)

# fake data for summary data display in Languages/Prosperity
nas <- rep(NA, 3)
tab2 <- data.frame(count = 1:3,
                   min = nas, Q1 = nas, median = nas, mean = nas, Q3 = nas, max = nas)

# plot filler
plotFill <- ggplot(mtcars, aes(wt, mpg)) + 
              annotate("text", x = 3.5, y = 20, size = 10, label = "Plot Here") +
              theme(axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank()) +
              labs(x = "x-axis title here", y = "y-axis title here")

# ui ------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "World Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Popular Languages", tabName = "languages1", icon = icon("globe")),
      menuItem("Language/Countries", tabName = "languages2", icon = icon("globe")),
      menuItem("Country/Languages", tabName = "languages3", icon = icon("globe")),
      menuItem("Language/Prosperity", tabName = "violins", icon = icon("usd")),
      menuItem("Scatterplots", tabName = "scatterplots", icon = icon("line-chart")),
      #menuItem("About", tabName = "other", icon = icon("file-text-o"))
      HTML(paste0('<li><a href="report.pdf" target="_blank">',
                  '<i class="fa fa-file-pdf-o" aria-hidden="true"></i> Documentation</a></li>')),
      HTML(paste0('<li><a href="https://github.com/homerhanumat/countries">',
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
                              choices=c("",sort(regions)))
                ),
                column(width = 9,
                  dataTableOutput("tblLanguages")
                )
              )
      ),
      tabItem(tabName = "languages2",
              fluidRow(
                column(width = 3,
                       helpText("For any language you select, you'll get a list of the ",
                                "countries in which it is spoken, in decreasing order of ",
                                "the percentage of the population that speaks the language. ",
                                "(Note: Some language and country names have ",
                                "had non-ascii characters deleted.)"),
                       uiOutput("languageFilter")
                ),
                column(width = 9,
                       dataTableOutput("tblLanguages2")
                )
              )
      ),
      tabItem(tabName = "languages3",
              fluidRow(
                column(width = 3,
                       helpText("For any country you select, you'll get a list of the ",
                                "languages spoken in it, in decreasing order of ",
                                "the percentage of the population that speaks the language. ",
                                "(Note: Some language and country names have ",
                                "had non-ascii characters deleted.)"),
                       uiOutput("countryFilter")
                ),
                column(width = 9,
                       dataTableOutput("tblLanguages3")
                )
              )
      ),
      tabItem(tabName = "violins",
              fluidRow(
                column(width = 3,
                       helpText("Here you can investigate the relationship (if any) ",
                                "between the language-diversity of a country and its ",
                                "prosperity.  You may choose to restrict to a particular ",
                                "region of the world."),
                       selectInput("response", label="Prosperity-Measure",
                                   choices=c("Per-Capita GNP" = "GNPpc",
                                             "Life Expectancy" = "LifeExpectancy")),
                       selectInput("region2", label="Restrict to a Region",  choices=c("",sort(regions))),
                       helpText(paste0("Consider a language to be 'common' in a country if it is",
                                       " spoken by at least the percentage you set below.\n",
                                       " The number of common languages in each",
                                       " country is used as a measure of its linguistic diversity.")),
                       numericInput("perc2", "Lower bound on percentage: ", 15, min = 0, max = 99)
                ),
                column(width = 9,
                       plotOutput("boxLanguages", click = "violin_click"),
                       tableOutput("tableLanguages"),
                       # div(id = "violinInfoWrapper",
                       #     style = "height: 130px; overflow: scroll;",
                       #     tableOutput("violinInfo"),
                       #     tableOutput("violinInfo2")
                       #     ),
                       div(id = "violinInfoPlaceholder", style = "height: 130px;",
                           helpText("Click near a point to identify the country."))
                )
              )
      ),
      tabItem(tabName = "scatterplots",
              fluidRow(
                column(width = 4,
                       helpText("You may select from four variables, each of which pertains to countries. ",
                                "The scatterplot shows the association between them."),
                       selectInput("pair", "Select two variables. (Click inside to see all choices.)", multiple = TRUE,
                              choices = c("Population Density" = "popDen",
                                          "Per-capita GNP" = "GNPpc",
                                          "Life Expectancy" = "LifeExpectancy",
                                          "Percentage Residing in Capital City" = "percCap"),
                              selected = c("popDen", "GNPpc")),
                       selectInput("region3", label="Filter by Region",  choices=c("",sort(regions))),
                       uiOutput("xyRanges"),
                       checkboxInput("smoother", "Add a Smoother", value = FALSE)
                ),
                column(width = 8,
                       plotOutput("scatterplot", click = "plot_click"),
                       br(),
                       # div(id = "plotInfoWrapper",
                       #     style = "height: 130px; overflow: scroll;",
                       #     uiOutput("plotInfo")
                       #     ),
                       div(id = "plotInfoPlaceholder", style = "height: 60px;",
                           helpText("Click near a point to identify the corresponding country."))
                       )
              )
      ),
      tabItem(tabName = "other",
              h2("Documentation Coming Soon!")
      )
    )
  )
)

# mock server ----------------------------------------

server <- function(input, output, session) {
  
# Tab 1:  Popular Languages ----------------------------  
    

  
  output$tblLanguages <- renderDataTable({
    tab
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10))

  
# Tab 2:  Language/Countries -----------------------    

  output$languageFilter <- renderUI({
    selectInput("language", "Select a language:", 
                choices = "Albanian")
  })

  output$tblLanguages2 <- renderDataTable({
    tab
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10)) 
  
# Tab 3:  Country/Languages ---------------------------------  
  
  output$countryFilter <- renderUI({
    selectInput("country", "Select a country:", 
                choices = "Afghanistan")
  })
  
  output$tblLanguages3 <- renderDataTable({
    tab
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10))
  

# Tab 4:  Language/Prosperity ------------------------------------
  
  
  output$boxLanguages <- renderPlot({
    plotFill
  })
  
  output$tableLanguages <- renderTable({
    tab2
  })
  

# Tab 5:  Scatterplots -----------------------------------
  
  output$scatterplot <- renderPlot({
    plotFill
  })
  
  output$xyRanges <- renderUI({
    tagList(
      sliderInput("xRange", "Restrict values of x-axis variable:",
                  min = 1, max = 5, value = c(1,5)),
      sliderInput("yRange", "Restrict values of y-axis variable:",
                  min = 0, max = 50, value = c(0,50))
    )
  })
  
}

# Run the App -------------------------------------

shinyApp(ui, server)
