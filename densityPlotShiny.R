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
                  choices = names(df)[c(155:181)],
                  selected = "prefers_st"),
      
      sliderInput(inputId ="ageRange", 
                  label ="Age Range:",
                  min = 15, max = 50,
                  value = c(15,50))
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
    df %>% 
      filter(df[,input$position] == "True") %>%
      filter(age >= input$ageRange[1], age <= input$ageRange[2]) %>%
      ggplot +
      aes_string(input$varComparing) +
      geom_density() +
      geom_vline(xintercept=df[df$name == input$player,input$varComparing],color = "blue") +
      coord_cartesian(xlim = c(0, 100)) 
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)