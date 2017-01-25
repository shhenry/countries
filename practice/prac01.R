library(DBI)
conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest")


city <- dbReadTable(conn, "City")
country <- dbReadTable(conn, "Country")
cl <- dbReadTable(conn,"CountryLanguage")

dbDisconnect(conn)
