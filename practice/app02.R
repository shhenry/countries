library(shiny)
library(DBI)

ui <- fluidPage(
  p("Enter up to three IDs:"),
  textInput("ID1", "First ID:", "5"),
  textInput("ID2", "Second ID:", ""),
  textInput("ID3", "Third ID:", ""),
  tableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinydemo",
      host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
      username = "guest",
      password = "guest")
    on.exit(dbDisconnect(conn), add = TRUE)
    sql <- "SELECT * FROM City WHERE ID = ?id1 OR ID = ?id2 OR ID = ?id3;"
    query <- sqlInterpolate(conn, sql, id1 = input$ID1,
                            id2 = input$ID2, id3 = input$ID3)
    dbGetQuery(conn, query)
  })
}

shinyApp(ui, server)
