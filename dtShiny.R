library(shiny)
library(DT)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  dataTableOutput('tbl')
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
    output$tbl = renderDataTable(df[,comparableStats], options = list(
    pageLength = 5, autoWidth = TRUE))
  }

# Create a Shiny app object
shinyApp(ui = ui, server = server)
