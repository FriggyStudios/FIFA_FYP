library(shiny)
library(DT)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  dataTableOutput('tbl')
)




# Define server function required to create the scatterplot
server <- function(input, output) {
  
    dfDT = df[,c(1:20)]
    output$tbl = renderDataTable(dfDT, options = list(
    pageLength = 5, autoWidth = TRUE))
  }

# Create a Shiny app object
shinyApp(ui = ui, server = server)
