library(shiny)
library(shinydashboard)
library(DBI)
library(pool)


pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest",
  # the three below are set at their default values:
  minSize = 1,
  maxSize = Inf,
  idleTimeout = 60000
)

regions <- c("Caribbean","Southern and Central Asia", "Central Africa",           
             "Southern Europe","Middle East","South America",          
             "Polynesia","Antarctica", "Australia and New Zealand",
             "Western Europe","Eastern Africa","Western Africa",         
             "Eastern Europe","Central America","North America",            
             "Southeast Asia", "Southern Africa","Eastern Asia",             
             "Nordic Countries","Northern Africa","Baltic Countries",         
             "Melanesia","Micronesia","British Islands",          
             "Micronesia/Caribbean")  

ui <- dashboardPage(
  dashboardHeader(title = "World Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Languages", tabName = "languages", icon = icon("world")),
      menuItem("Histograms", tabName = "histograms", icon = icon("area-chart")),
      menuItem("Scatterplots", tabName = "scatterplots", icon = icon("line-chart")),
      menuItem("About", tabName = "other", icon = icon("document"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "languages",
              fluidRow(
                column(width = 3,
                  selectInput("region", label="Filter by Region",  choices=c("",sort(regions))),
                  helpText(paste0("You may arrange so as count speakers of a language if that",
                                  " language is \"commonly-spoken\" in their country, i.e.:",
                                  " they constitute at least a set percentage of the population",
                                  " of the country.  Choose that percentage below.")),
                  numericInput("perc", "Lower bound on percentage: ", 0)
                ),
                column(width = 9,
                  dataTableOutput("tblLanguages")
                )
              )
      ),
      
      tabItem(tabName = "histograms",
              h2("Other content")
      ),
      tabItem(tabName = "scatterplots",
              h2("Other content")
      ),
      tabItem(tabName = "other",
              h2("Other content")
      )
    )
  )
)

server <- function(input, output, session) {
  output$tblLanguages <- renderDataTable({
    validate(need(input$perc != "", 
                  label = "Waiting:  the percentage")
    )
    filterRegion <- input$region != ""
    sql <- paste0("select Language, sum(Percentage/100*Population) as TotalSpeakers from\n",
            "(select * from (select * from CountryLanguage where Percentage > ?perc) as poplan\n")
            
    if ( !filterRegion ) {
      sql <- paste0(sql,
                     "inner join Country\n")
    } else {
      sql <- paste0(sql,"inner join (select Code, Population from Country where Region=?region) as Country\n") 
      }
    sql <- paste0(sql, "on poplan.CountryCode=Country.Code) as ccl\n",
            "group by Language\n",
            "order by TotalSpeakers desc;")
    if ( filterRegion ) {
      query <- sqlInterpolate(pool, sql, perc = input$perc, region = input$region)
    } else {
      query <- sqlInterpolate(pool, sql, perc = input$perc)
    }
    tab <- dbGetQuery(pool, query)
    languages <- tab$Language
    Encoding(languages) <- "latin1"
    languages <- iconv(languages, "latin1", "ASCII", sub="")
    tab$Language <- languages
    ranks <- 1:nrow(tab)
    tab2 <- cbind(ranks, tab)
    names(tab2)[1] <- "Rank"
    names(tab2)[3] <- "Total Speakers"
    tab2
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10))
  
}

shinyApp(ui, server)
