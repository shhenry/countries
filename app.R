# library and globals ---------------------

library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(ggplot2)
library(shinyjs)


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


# CountryLanguages
sql <- paste0("select * from CountryLanguage\n",
              "inner join Country\n",
              "on CountryLanguage.CountryCode=Country.Code;")
CountryLanguages <- dbGetQuery(pool, sql)
# remove non-ascii characters from language names
languages <- CountryLanguages$Language
Encoding(languages) <- "latin1"
languages <- iconv(languages, "latin1", "ASCII", sub="")
CountryLanguages$Language <- languages
# remove non-ascii characters from country names
names <- CountryLanguages$Name
Encoding(names) <- "latin1"
names <- iconv(names, "latin1", "ASCII", sub="")
CountryLanguages$Name <- names

# ScatterTable
# for the scatterplots tab we'll go ahead and get one table up front
sql <- paste0("select Name, SurfaceArea, Region, Continent, Population, LifeExpectancy, GNP, Capital, IndepYear, popCap from Country\n",
              "inner join (select ID, Population as popCap from City) as City\n",
              "on Country.Capital=City.ID;")
scatterTable <- dbGetQuery(pool, sql)
scatterTable$popDen <- with(scatterTable, round(Population/SurfaceArea,3))
scatterTable$GNPpc <- with(scatterTable, round(GNP/Population*1000000, 0))
scatterTable$percCap <- with(scatterTable, round(popCap/Population*100,1))
# remove non-ascii characters from country names
names <- scatterTable$Name
Encoding(names) <- "latin1"
names <- iconv(names, "latin1", "ASCII", sub="")
scatterTable$Name <- names



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
      HTML(paste0('<li><a href="https://github.com/homerhanumat/countries" target="_blank">',
                  '<i class="fa fa-github" aria-hidden="true"></i> Source Code</a></li>'))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
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
                       div(id = "violinInfoWrapper",
                           style = "height: 130px; overflow: scroll;",
                           tableOutput("violinInfo"),
                           tableOutput("violinInfo2")
                           ),
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
                       div(id = "plotInfoWrapper",
                           style = "height: 130px; overflow: scroll;",
                           uiOutput("plotInfo")
                           ),
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

# server ----------------------------------------

server <- function(input, output, session) {

# reactive values ----------------------------------------------- 
  # tab1 = Popular Languages, tab2 = language/country, tab3 = country/languages
  # tab4 = langProsperity, tab5 = otherPairs
  # resp gives response variable (GNPpc or Life expectancy) in langProsperity
  rv <- reactiveValues(tab1 = NULL,
                       tab2 = NULL,
                       tab3 = NULL,
                       tab4 = NULL,
                       tab5 = scatterTable,
                       resp = NULL)
  


  
# Tab 1:  Popular Languages ----------------------------  
    
  #observer for languages1:  create sql query, run it, store returned table
  observe({
    filterRegion <- input$region != ""
    sql <- paste0("select Language, sum(Percentage/100*Population) as TotalSpeakers from\n",
                  "(select * from CountryLanguage\n")
    if ( !filterRegion ) {
      sql <- paste0(sql,
                    "inner join Country\n")
    } else {
      sql <- paste0(sql,"inner join (select Code, Population from Country where Region=?region) as Country\n") 
    }
    sql <- paste0(sql, "on CountryLanguage.CountryCode=Country.Code) as ccl\n",
                  "group by Language\n",
                  "order by TotalSpeakers desc;")
    if ( filterRegion ) {
      query <- sqlInterpolate(pool, sql, region = input$region)
    } else {
      query <- sql
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
  
  output$tblLanguages <- renderDataTable({
    rv$tab1
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10))

  
# Tab 2:  Language/Countries -----------------------    

  output$languageFilter <- renderUI({
    selectInput("language", "Select a language:", 
                choices = sort(unique(CountryLanguages$Language)))
  })
  
  # observer for languages2
  observe({
    rv$tab2 <- subset(CountryLanguages, Language == input$language)
  })

  output$tblLanguages2 <- renderDataTable({
    tab <- rv$tab2[order(rv$tab2$Percentage, decreasing = TRUE), ]
    if ( !is.null(tab) ) {
      ranks <- 1:nrow(tab)
      tab <- cbind(ranks, tab)
      names(tab)[1] <- "Rank"
      tab[, c("Rank", "Name", "Percentage")]
    }
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10)) 
  
# Tab 3:  Country/Languages ---------------------------------  
  
  # easier for me to find this code here rather than in the output section:
  output$countryFilter <- renderUI({
    selectInput("country", "Select a country:", 
                choices = sort(unique(CountryLanguages$Name)))
  })
  
  # observer for languages3
  observe({
    rv$tab3 <- subset(CountryLanguages, Name == input$country)
  })
  
  output$tblLanguages3 <- renderDataTable({
    tab <- rv$tab3[order(rv$tab3$Percentage, decreasing = TRUE), ]
    if ( !is.null(tab) ) {
      ranks <- 1:nrow(tab)
      tab <- cbind(ranks, tab)
      names(tab)[1] <- "Rank"
      tab[, c("Rank", "Language", "Percentage")]
    }
  }, options = list(lengthMenu = c(10, 15, 20), pageLength = 10))
  

# Tab 4:  Language/Prosperity ------------------------------------
  
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
    sql <- paste0("select ", responseCode, ", count(Percentage) as count, Code, Name, Region, Continent, IndepYear from\n",
                  "(select * from (select * from CountryLanguage where Percentage >= ?perc) as poplan\n")
    if ( !filterRegion ) {
      sql <- paste0(sql,"inner join Country\n")
    } else {
      sql <- paste0(sql,"inner join (select Code, Population, GNP, LifeExpectancy, Name, Region, Continent, IndepYear from Country where Region=?region) as Country\n") 
    }
    sql <- paste0(sql, "on poplan.CountryCode=Country.Code) as ccl\n",
                  "group by Code;")
    if ( filterRegion ) {
      query <- sqlInterpolate(pool, sql, perc = input$perc2, region = input$region2)
    } else {
      query <- sqlInterpolate(pool, sql, perc = input$perc2)
    }
    tab <- dbGetQuery(pool, query)
    tab$count <- factor(tab$count)
    
    # let's try to track the jittered points
    set.seed(2020)
    if ( rv$resp == "GNPpc" ) {
      p <- ggplot(tab, aes(count, GNPpc)) + geom_violin() + geom_jitter(width = 0.2)
    } else {
      p <- ggplot(tab, aes(count, LifeExpectancy)) + geom_violin() + geom_jitter(width = 0.2)
    }
    tab$xPoints <- layer_data(p, i = 2)$x
    
    # remove non-ascii characters from language names
    names <- tab$Name
    Encoding(names) <- "latin1"
    names <- iconv(names, "latin1", "ASCII", sub="")
    tab$Name <- names
    
    rv$tab4 <- tab
  })
  
  output$boxLanguages <- renderPlot({
    if ( rv$resp == "GNPpc" ) {
      p <- ggplot(rv$tab4, aes(count, GNPpc))
      p  + geom_violin(fill = "burlywood") + geom_point(aes(xPoints, GNPpc)) +
        labs(title = "Per Capita GNP, by Language-Diversity",
             x = paste0("Number of languages spoken by at least ",input$perc2,"% of the population"),
             y = "Per-capita Gross National Product (dollars)")
    } else {
      p <- ggplot(rv$tab4, aes(count, LifeExpectancy))
      p  + geom_violin(fill = "burlywood") + geom_point(aes(xPoints, LifeExpectancy)) +
        labs(title = "Life Expectancy, by Language-Diversity",
             x = paste0("Number of languages spoken by at least ",input$perc2,"% of the population"),
             y = "Life Expectancy (years)")
    }
  })
  
  
  output$violinInfo <- renderTable({
    df <- nearPoints(rv$tab4, xvar = "xPoints", input$violin_click, threshold = 10, maxpoints = 1)
    if ( nrow(df) > 0 ) {
      show("violinInfoWrapper")
      hide("violinInfoPlaceholder")
    } else {
      hide("violinInfoWrapper")
      show("violinInfoPlaceholder")
    }
    df[, c("Name", "Region", "Continent", "IndepYear")]
  })
  
  outputOptions(output, 'violinInfo', suspendWhenHidden = FALSE)
  
  output$violinInfo2 <- renderTable({
    validate(need(!is.null(input$perc2), message = ""))
    df <- nearPoints(rv$tab4, xvar = "xPoints", input$violin_click, threshold = 10, maxpoints = 1)
    if (nrow(df) > 0 ) {
      countryName <- df$Code
      tab <- subset(CountryLanguages, Code == countryName & Percentage >= input$perc2)
      print(tab)
      print(order(tab$Percentage, decreasing = TRUE))
      tab <- tab[order(tab$Percentage, decreasing = TRUE), ]
      tab[, c("Language", "Percentage")]
    }
  })
  
  outputOptions(output, 'violinInfo2', suspendWhenHidden = FALSE)
  
  output$tableLanguages <- renderTable({
    tab <- rv$tab4
    tab$count <- factor(tab$count)
    response <- ifelse(rv$resp == "GNPpc","GNPpc","LifeExpectancy")
    
    q1 <- function(x) quantile(x, 0.25)
    q3 <- function(x) quantile(x, 0.75)
    
    aggMin <- aggregate(tab[, response] ~ count, data = tab, FUN = min, na.action = na.omit)
    aggQ1 <- aggregate(tab[, response] ~ count, data = tab, FUN = q1, na.action = na.omit)
    aggMed <- aggregate(tab[, response] ~ count, data = tab, FUN = median, na.action = na.omit)
    aggMean <- aggregate(tab[, response] ~ count, data = tab, FUN = mean, na.action = na.omit)
    aggQ3 <- aggregate(tab[, response] ~ count, data = tab, FUN = q3, na.action = na.omit)
    aggMax <- aggregate(tab[, response] ~ count, data = tab, FUN = max, na.action = na.omit)
    
    aggTable <- cbind(aggMin, aggQ1[, 2], aggMed[ ,2], aggMean[ ,2], aggQ3[ ,2], aggMax[ ,2])
    colnames(aggTable) <- c("Languages", "Min", "Q1", "Median", "Mean", "Q3", "Max")
    aggTable
  })
  

# Tab 5:  Scatterplots -----------------------------------
  
  # observer for scatterplots
  observe({
    filterRegion <- input$region3 != ""
    if ( filterRegion ) {
      rv$tab5 <- subset(scatterTable, Region == input$region3)
    } else {
      rv$tab5 <- scatterTable
    }
  })
  
  output$scatterplot <- renderPlot({
    validate(need(length(input$pair) == 2, 
                  message = "Please select exactly two distinct variables.")
    )
    xName <- input$pair[1]
    yName <- input$pair[2]
    x <- get(xName, envir = as.environment(rv$tab5))
    y <- get(yName, envir = as.environment(rv$tab5))
    df <- data.frame(x,y)
    names(df) <- c(xName, yName)
    xMin <- input$xRange[1]
    xMax <- input$xRange[2]
    yMin <- input$yRange[1]
    yMax <- input$yRange[2]
    inRange <- x >= xMin & x <= xMax & y >= yMin & y <= yMax
    df <- subset(df, inRange)
    
    xLabel <- switch(xName,
                     popDen = "Population density (persons per square km)",
                     percCap = "Percentage residing in the capital city",
                     GNPpc = "Per-capita GNP (dollars)",
                     LifeExpectancy = "Life Expectancy (years)"
                     )
    yLabel <- switch(yName,
                     popDen = "Population density (persons per square km)",
                     percCap = "Percentage residing in the capital city",
                     GNPpc = "Per-capita GNP (dollars)",
                     LifeExpectancy = "Life Expectancy (years)"
    )
                      
    
    p <- ggplot(df, aes_string(xName, yName)) + geom_point() + labs(x = xLabel, y = yLabel)
    if ( input$smoother ) {
      p <- p + geom_smooth()
    }
    p
  })
  
  output$plotInfo <- renderTable({
    df <- nearPoints(rv$tab5, input$plot_click, threshold = 10, maxpoints = 1)
    if ( nrow(df) == 0 ) {
      hide("plotInfoWrapper")
      show("plotInfoPlaceholder")
    } else {
      show("plotInfoWrapper")
      hide("plotInfoPlaceholder")
    }
    df[, c("Name", "Region", "Continent", "IndepYear")]
  })
  
  outputOptions(output, 'plotInfo', suspendWhenHidden = FALSE)
  
  output$xyRanges <- renderUI({
    validate(need(length(input$pair) == 2, 
                  message = "")
    )
    tab <- rv$tab5
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

# Run the App -------------------------------------

shinyApp(ui, server)
