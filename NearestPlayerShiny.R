library(shiny)
library(DT)
library(dplyr)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      selectInput(inputId = "player",
                  label = "Player",
                  choices = df[,2]),
      
      selectInput(inputId = "position",
                  label = "Position",
                  choices = c(names(df)[c(156,160,163,165:170,172,173,181)]),
                  selected = c(),
                  multiple = T),
      
      selectInput(inputId = "nationality",
                  label = "Nationality",
                  choices = c(levels(df$nationality)),
                  selected = c(),
                  multiple = T),
      
      selectInput(inputId = "league",
                  label = "League",
                  choices = c(levels(df$league)),
                  selected = c(),
                  multiple = T),
      
      selectInput(inputId = "club",
                  label = "Club",
                  choices = c(levels(df$club)),
                  selected = c(),
                  multiple = T),    
      sliderInput(inputId ="ageRange", 
                  label ="Age Range:",
                  min = 15, max = 50,
                  value = c(15,50)),
      
      sliderInput(inputId ="valueRange", 
                  label ="Value Range(Millions):",
                  min = 0, max = 150,
                  value = c(0,150))
    ),
    
    # Output
    mainPanel(
      dataTableOutput(outputId = "tbl")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  output$tbl =
    renderDataTable(df %>%
                      filter(full_name != input$player) %>%
                      filterPlayers(input$ageRange,
                                    input$valueRange,
                                    input$position,
                                    input$nationality,
                                    input$league,
                                    input$club) %>%
                      nearest(df[df$full_name == input$player,],
                              df$prefers_gk[df$full_name == input$player]),
      options = list(
    pageLength = 10, autoWidth = TRUE))
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)