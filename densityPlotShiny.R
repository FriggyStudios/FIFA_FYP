library(shiny)
library(ggplot2)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      selectInput(inputId = "player",
                  label = "Player",
                  choices = df[,1],
                  selected = "prefers_st"),
      
      selectInput(inputId = "varComparing", 
                  label = "Variable Comparing",
                  choices = names(df)[c(19:23,30:63)],
                  selected = "pac"),
      
      selectInput(inputId = "position",
                  label = "Position",
                  choices = c("any",names(df)[c(156,160,163,165:170,172,173,181)]),
                  selected = "any"),
      
      selectInput(inputId = "nationality",
                  label = "Nationality",
                  choices = c("any",levels(df$nationality)),
                  selected = "any"),
      
      selectInput(inputId = "league",
                 label = "League",
                 choices = c("any",levels(df$league)),
                 selected = "any"),
      
      selectInput(inputId = "club",
                 label = "Club",
                 choices = c("any",levels(df$club)),
                 selected = "any"),
      
      sliderInput(inputId ="ageRange", 
                  label ="Age Range:",
                  min = 15, max = 50,
                  value = c(15,50)),
      
      sliderInput(inputId ="valueRange", 
                  label ="Value Range(Millions):",
                  min = 0, max = 150,
                  value = c(0,50))
    ),
    
    # Output
    mainPanel(
      plotOutput(outputId = "densityplot")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the densityplot object the plotOutput function is expecting
  output$densityplot <- renderPlot({
    df %>% filterPlayers(input$ageRange,
                         input$valueRange,
                         c(input$position),
                         c(input$nationality),
                         c(input$league),
                         c(input$club)) %>%
      ggplot +
      aes_string(input$varComparing) +
      geom_density() +
      geom_vline(xintercept=df[df$name == input$player,input$varComparing],color = "blue") +
      coord_cartesian(xlim = c(0, 100)) 
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)