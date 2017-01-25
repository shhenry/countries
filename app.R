library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(ggplot2)


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
      menuItem("Popular Languages", tabName = "languages", icon = icon("globe")),
      menuItem("Language Diversity", tabName = "violins", icon = icon("usd")),
      menuItem("Scatterplots", tabName = "scatterplots", icon = icon("line-chart")),
      menuItem("About", tabName = "other", icon = icon("file-text-o"))
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
                  numericInput("perc", "Lower bound on percentage: ", 0, min=0, max = 99)
                ),
                column(width = 9,
                  dataTableOutput("tblLanguages")
                )
              )
      ),
      
      tabItem(tabName = "violins",
              fluidRow(
                column(width = 3,
                       selectInput("response", label="Prosperity-Measure",
                                   choices=c("Per-Capita GNP" = "GNPpc",
                                             "Life Expectancy" = "LifeExpectancy")),
                       selectInput("region2", label="Filter by Region",  choices=c("",sort(regions))),
                       helpText(paste0("Consider a language to be 'common' in a country if it is",
                                       " spoken by at least the percentage you set below.\n",
                                       " We will count the number of common languages in each",
                                       " country.")),
                       numericInput("perc2", "Lower bound on percentage: ", 15, min = 0, max = 99)
                ),
                column(width = 9,
                       plotOutput("boxLanguages"),
                       br(),
                       tableOutput("tableLanguages")
                )
              )
      ),
      tabItem(tabName = "scatterplots",
              fluidRow(
                column(width = 4,
                       selectInput("pair", "Choose a pair of variables:", multiple = TRUE,
                              choices = c("Population Density" = "popDen",
                                          "Per-capita GNP" = "GNPpc",
                                          "Life Expectancy" = "LifeExpectancy",
                                          "Percentage Residing in Capital City" = "percCap"),
                              selected = c("popDen", "GNPpc")),
                       selectInput("region3", label="Filter by Region",  choices=c("",sort(regions))),
                       uiOutput("xyRanges")
                ),
                column(width = 8,
                       plotOutput("scatterplot", hover = "plot_hover"),
                       br(),
                       helpText("Hover over a point:  the corresponding country will be identified below."),
                       tableOutput("plotInfo")
                       )
              )
      ),
      tabItem(tabName = "other",
              h2("Documentation Coming Soon!")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # tab1 = languages, tab2 = langProsperity, tab3 = otherPairs
  # resp gives response variable (GNPpc or Life expectancy) in langProsperity
  rv <- reactiveValues(tab1 = NULL,
                       tab2 = NULL,
                       tab3 = NULL,
                       resp = NULL)
  
  #observer for languages:  create sql query, run it, store returned table
  observe({
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
    
    # remove non-ascii characters from language names
    languages <- tab$Language
    Encoding(languages) <- "latin1"
    languages <- iconv(languages, "latin1", "ASCII", sub="")
    tab$Language <- languages
    
    # create ranks
    ranks <- 1:nrow(tab)
    tab1 <- cbind(ranks, tab)
    names(tab1)[1] <- "Rank"
    
    # better name for column:
    names(tab1)[3] <- "Total Speakers"
    
    # store in reactive values
    rv$tab1 <- tab1
  })
  
  # observer for langProsperity
  observe({
    validate(need(input$perc2 != "", 
                  label = "Waiting:  the percentage")
    )
    filterRegion <- input$region2 != ""
    responseCode <- ifelse(input$response == "GNPpc",
                               "GNP/Population*1000000 as GNPpc",
                               "LifeExpectancy")
    rv$resp <- ifelse(input$response == "GNPpc",
                      "GNPpc",
                      "life")
    sql <- paste0("select ", responseCode, ", count(Percentage) as count from\n",
                  "(select * from (select * from CountryLanguage where Percentage >= ?perc) as poplan\n")
    if ( !filterRegion ) {
      sql <- paste0(sql,"inner join Country\n")
    } else {
      sql <- paste0(sql,"inner join (select Code, Population, GNP, LifeExpectancy from Country where Region=?region) as Country\n") 
    }
    sql <- paste0(sql, "on poplan.CountryCode=Country.Code) as ccl\n",
                  "group by Code;")
    if ( filterRegion ) {
      query <- sqlInterpolate(pool, sql, perc = input$perc2, region = input$region2)
    } else {
      query <- sqlInterpolate(pool, sql, perc = input$perc2)
    }
    rv$tab2 <- dbGetQuery(pool, query)
  })
  
  # for the scatterplots tab we'll go ahead and get one table up front
  sql <- paste0("select Name, SurfaceArea, Region, Continent, Population, LifeExpectancy, GNP, Capital, IndepYear, popCap from Country\n",
                "inner join (select ID, Population as popCap from City) as City\n",
                "on Country.Capital=City.ID;")
  scatterTable <- dbGetQuery(pool, sql)
  scatterTable$popDen <- with(scatterTable, round(Population/SurfaceArea,3))
  scatterTable$GNPpc <- with(scatterTable, round(GNP/Population*1000000, 0))
  scatterTable$percCap <- with(scatterTable, round(popCap/Population*100,1))
  rv$tab3 <- scatterTable
  
  # observer for scatterplots
  observe({
    filterRegion <- input$region3 != ""
    if ( filterRegion ) {
      rv$tab3 <- subset(scatterTable, Region == input$region3)
    } else {
      rv$tab3 <- scatterTable
    }
  })
  
  output$tblLanguages <- renderDataTable({
    rv$tab1
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10))
  
  
  output$boxLanguages <- renderPlot({
    if ( rv$resp == "GNPpc" ) {
      p <- ggplot(rv$tab2, aes(factor(count), GNPpc))
      p  + geom_violin(fill = "burlywood") + geom_jitter(width = 0.2) +
              labs(title = "Per Capita GNP, by Language-Diversity",
                  x = paste0("Number of languages spoken by at least ",input$perc2,"% of the population"),
                  y = "Per-capita Gross National Product (dollars)")
    } else {
      p <- ggplot(rv$tab2, aes(factor(count), LifeExpectancy))
      p  + geom_violin(fill = "burlywood") + geom_jitter(width = 0.2) +
              labs(title = "Life Expectancy, by Language-Diversity",
                   x = paste0("Number of languages spoken by at least ",input$perc2,"% of the population"),
                   y = "Life Expectancy (years)")
    }
  })
  
  output$tableLanguages <- renderTable({
    tab <- rv$tab2
    tab$count <- factor(tab$count)
    response <- ifelse(rv$resp == "GNPpc","GNPpc","LifeExpectancy")
    
    q1 <- function(x) quantile(x, 0.25)
    q3 <- function(x) quantile(x, 0.75)
    
    aggMin <- aggregate(tab[, response], by = list(tab$count), FUN = min)
    aggQ1 <- aggregate(tab[, response], by = list(tab$count), FUN = q1)
    aggMed <- aggregate(tab[, response], by = list(tab$count), FUN = median)
    aggMean <- aggregate(tab[, response], by = list(tab$count), FUN = mean)
    aggQ3 <- aggregate(tab[, response], by = list(tab$count), FUN = q3)
    aggMax <- aggregate(tab[, response], by = list(tab$count), FUN = max)
    
    aggTable <- cbind(aggMin, aggQ1[, 2], aggMed[ ,2], aggMean[ ,2], aggQ3[ ,2], aggMax[ ,2])
    colnames(aggTable) <- c("Languages", "Min", "Q1", "Median", "Mean", "Q3", "Max")
    aggTable
  })
  
  output$scatterplot <- renderPlot({
    validate(need(length(input$pair) == 2, 
                  message = "Please select exactly two distinct variables.")
    )
    xName <- input$pair[1]
    yName <- input$pair[2]
    x <- get(xName, envir = as.environment(rv$tab3))
    y <- get(yName, envir = as.environment(rv$tab3))
    df <- data.frame(x,y)
    names(df) <- c(xName, yName)
    xMin <- input$xRange[1]
    xMax <- input$xRange[2]
    yMin <- input$yRange[1]
    yMax <- input$yRange[2]
    inRange <- x >= xMin & x <= xMax & y >= yMin & y <= yMax
    df <- subset(df, inRange)
    p <- ggplot(df, aes_string(xName, yName))
    p + geom_point()
  })
  
  output$plotInfo <- renderTable({
    df <- nearPoints(rv$tab3, input$plot_hover, threshold = 10, maxpoints = 1)
    if ( nrow(df) >0 ) df[, c("Name", "Region", "Continent", "IndepYear")]
  })
  
  output$xyRanges <- renderUI({
    validate(need(length(input$pair) == 2, 
                  message = "")
    )
    tab <- rv$tab3
    x <- tab[, input$pair[1]]
    y <- tab[, input$pair[2]]
    minX <- min(x, na.rm = T)
    minY <- min(y, na.rm = T)
    maxX <- max(x, na.rm = T)
    maxY <- max(y, na.rm = T)
    tagList(
      sliderInput("xRange", "Restrict values of x-axis variable:",
                  min = minX, max = maxX, value = c(minX, maxX)),
      sliderInput("yRange", "Restrict values of y-axis variable:",
                  min = minY, max = maxY, value = c(minY, maxY))
    )
  })
  
}

shinyApp(ui, server)
