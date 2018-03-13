library(shiny)
library(DT)
library(dplyr)
library(fmsb)
library(class)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      checkboxInput(inputId = "findMe", label = "Find Me", value = FALSE),
      checkboxInput(inputId = "render", label = "Render", value = TRUE),
      tabsetPanel(type = "tabs",
                  tabPanel("Player",
                           uiOutput(outputId = "playerSearch")),
                  tabPanel("Weights",
                           uiOutput(outputId = 'weights')),
                  tabPanel("Me!",
                           uiOutput(outputId = "mePosition"),
                           uiOutput(outputId = "findMeStats")
                           ),
                  tabPanel("Subsetting",
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
                                       value = c(18,35)),
                           
                           sliderInput(inputId ="valueRange", 
                                       label ="Value Range(Millions):",
                                       min = 0, max = 150,
                                       value = c(5,150))
                  ))),
    
    # Output
    mainPanel(
      conditionalPanel( condition = "!input.findMe && input.render",
        dataTableOutput(outputId = "tblSearch")
      ),
      conditionalPanel( condition = "input.findMe && input.render",
        dataTableOutput(outputId = "tblFindMe")
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output,session) {
  
  
  output$mePosition <- renderUI({
  selectInput(inputId = 'myPosition',
              label = 'Your Position',
              choices = c(names(df)[c(156,160,163,165:170,172,173,181)]),
              selected = "prefers_st")
  })
  
  output$findMeStats <- renderUI({
      #My stats
      lapply(30:63, function(i) {
        sliderInput(inputId = paste0('myStat', i),
                    label = names(df)[i],
                    min = 0, max = 99,
                    value = 0)
      })
  })
  
  output$playerSearch <- renderUI({
      selectInput(inputId = "player",
                  label = "Player",
                  choices = df[,2],
                  selected = "Lionel Messi")
  })
  
  
  # The dynamic input definition
  output$weights <- renderUI({
    
      #Weights
      lapply(30:63, function(i) {
        sliderInput(inputId = paste0('weight', i),
                    label = paste0(names(df)[i]," weight"),
                    min = 0, max = 1,
                    value = 0.5)
      })
    
  })
    weights <- reactive({c( lapply(1:29,function(i){i}),
                                    lapply(30:63,function(i){
                                      if(is.null(input[[paste0("weight",i)]])){
                                        return(1)
                                      }
                                      else { return (input[[paste0("weight",i)]])}
                                      }),
                                    lapply(64:181,function(i){i}))})
    
      searchPlayerCompare <- reactive({df[df$full_name == input$player,]})
      output$tblSearch =
    renderDataTable((df  %>%
                      filterPlayers(input$ageRange,
                                    input$valueRange,
                                    input$position,
                                    input$nationality,
                                    input$league,
                                    input$club) %>%
                      nearest(searchPlayerCompare(),weights()))[,comparableStats],
      options = list(
    pageLength = 10, autoWidth = TRUE))
      
    myPlayerCompare <- reactive({c( lapply(1:29,function(i){i}),
      lapply(30:63,function(i){if(is.null(input[[paste0("myStat",i)]])){
        return(0)
      }
        else { return (input[[paste0("myStat",i)]])}
      }),
      lapply(64:180,function(i){i}),
      lapply(181,function(i){
      if(input$myPosition == "prefers_gk")
        'True'
      else
        'False'
    }),lapply(-1,function(i){i}))
    })
    
    output$tblFindMe =
      renderDataTable((df %>%
                         filterPlayers(input$ageRange,
                                       input$valueRange,
                                       input$position,
                                       input$nationality,
                                       input$league,
                                       input$club) %>%
                         nearest(myPlayerCompare(),weights()))[,comparableStats],
      options = list(
        pageLength = 10, autoWidth = TRUE))
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)