library(shiny)
library(DT)
library(dplyr)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
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
                  selected = c("Liverpool"),
                  multiple = T),
      
      sliderInput(inputId ="ageRange", 
                  label ="Age Range:",
                  min = 15, max = 50,
                  value = c(18,35)),
      
      sliderInput(inputId ="valueRange", 
                  label ="Player Value Range(Millions):",
                  min = 0, max = 150,
                  value = c(5,150))
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
    renderDataTable(
      (makeTeam(
      (ordered_gk %>%
          filterPlayers(ageRange = input$ageRange,
                       value = input$valueRange,
                       nationalities= input$nationality,
                       leagues = input$league,
                       clubs = input$club)),
      (ordered_cb %>%
        filterPlayers(ageRange = input$ageRange,
                      value = input$valueRange,
                      nationalities= input$nationality,
                      leagues = input$league,
                      clubs = input$club)),
      (ordered_lb %>%
        filterPlayers(ageRange = input$ageRange,
                      value = input$valueRange,
                      nationalities= input$nationality,
                      leagues = input$league,
                      clubs = input$club)),
  (ordered_rb %>%
    filterPlayers(ageRange = input$ageRange,
                  value = input$valueRange,
                  nationalities= input$nationality,
                  leagues = input$league,
                  clubs = input$club)),
(ordered_cm %>%
  filterPlayers(ageRange = input$ageRange,
                value = input$valueRange,
                nationalities= input$nationality,
                leagues = input$league,
                clubs = input$club)),
(ordered_lm %>%
  filterPlayers(ageRange = input$ageRange,
                value = input$valueRange,
                nationalities= input$nationality,
                leagues = input$league,
                clubs = input$club)),
(ordered_rm %>%
  filterPlayers(ageRange = input$ageRange,
                value = input$valueRange,
                nationalities= input$nationality,
                leagues = input$league,
                clubs = input$club)),
(ordered_st %>%
  filterPlayers(ageRange = input$ageRange,
                value = input$valueRange,
                nationalities= input$nationality,
                leagues = input$league,
                clubs = input$club)) )
      
      ),
                    options = list(
                      pageLength = 11, autoWidth = TRUE))
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
