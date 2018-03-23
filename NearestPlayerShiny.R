library(shiny)
library(DT)
library(dplyr)
library(fmsb)
library(class)


predictPosition <- function(nearestToMe){
  position <- c()
  for(i in 64:90){
    position[i] <- 0
    for(j in 1:length(nearestToMe[1])){
     if(nearestToMe[[j,i]] == "True"){
     position[i] <- as.numeric(position[i]) + 1
    }
     }
  }
  paste("Predicted position: ",(names(df))[which.max(position)-1])
}
predictValue <- function(nearestToMe){
  value <- 0
  i = 14
  for(j in 1:length(nearestToMe[1])){
    value <- value + as.numeric(nearestToMe[[j,i]])
  }
  value <- value/length(nearestToMe[1])
  paste("Predicted value: ",value," Euro")
}

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      checkboxInput(inputId = "render", label = "Render", value = FALSE),
      selectInput(inputId = "player",
                  label = "Player Compare",
                  choices = df[,2],
                  selected = "Lionel Messi"),
      selectInput(inputId = "searchType",
         label = "Search Type",
         choices = c("Find Me","Player"),
         selected = c("Find Me")), 
      tabsetPanel(type = "tabs",
                  tabPanel("Me!",
                           uiOutput(outputId = "mePosition"),
                           uiOutput(outputId = "findMeStats")
                  ),
                  tabPanel("Weights",
                           uiOutput(outputId = 'weights')),
                  tabPanel("Subsetting",
                           selectInput(inputId = "position",
                                       label = "Position",
                                       choices = c(names(df)[c(65,69,72,74:79,81,82,90)]),
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
      tabsetPanel(type = "tabs",
                  tabPanel("Table",
        conditionalPanel( condition = "input.searchType != 'Find Me' && input.render",
          dataTableOutput(outputId = "tblSearch")
        ),
        conditionalPanel( condition = "input.searchType == 'Find Me' && input.render",
          textOutput("PredictedPosition"),
          textOutput("PredictedValue"),
          dataTableOutput(outputId = "tblFindMe")
        )),
                tabPanel("Radar",
        conditionalPanel( condition = "input.searchType != 'Find Me' && input.render",
          plotOutput('radarSearch'),
          textOutput("radarOutputSearch"),
          uiOutput(outputId = "radarSearchCompare")
        ),
        conditionalPanel( condition = "input.searchType == 'Find Me' && input.render",
          plotOutput('radarFindMe'),
          textOutput("radarOutputMe"),
          uiOutput(outputId = "radarMeCompare")
        )
        )
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output,session) {
  
  output$radarOutputSearch <- renderText({"Blue: Player Searching, Red: Player Comparing"})
  output$radarOutputMe <- renderText({"Blue: Player You, Red: Player Comparing"})
  output$mePosition <- renderUI({
  selectInput(inputId = 'myPosition',
              label = 'Your Position',
              choices = c(names(df)[c(65,69,72,74:79,81,82,90)]),
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
                      nearest(searchPlayerCompare(),weights()))[,c(1,3,4,7,8,9,10,11,13,14,15,17:65)],
      options = list(
    pageLength = 10, autoWidth = TRUE))
      
      
    myPlayerCompare <- reactive({c( lapply(1:29,function(i){i}),
      lapply(30:63,function(i){if(is.null(input[[paste0("myStat",i)]])){
        return(0)
      }
        else { return (input[[paste0("myStat",i)]])}
      }),
      lapply(64:90,function(i){i}),
      lapply(9,function(i){
      if(input$myPosition == "prefers_gk")
        'True'
      else
        'False'
    }),lapply(-1,function(i){i}))
    })
    
    nearestToMe <- reactive({df %>%
        filterPlayers(input$ageRange,
                      input$valueRange,
                      input$position,
                      input$nationality,
                      input$league,
                      input$club) %>%
        nearest(myPlayerCompare(),weights())})
    
    output$PredictedPosition <- renderText({ predictPosition(nearestToMe()) })
    output$PredictedValue <- renderText({ predictValue(nearestToMe()) })
     
    output$tblFindMe =
      renderDataTable((
                         nearestToMe())[,c(1,3,4,7,8,9,10,11,13,14,15,17:65)],
      options = list(
        pageLength = 10, autoWidth = TRUE))
    
    output$radarSearchCompare <- renderUI({
      selectInput(inputId = "radarSearchCompare",
                  label = "Compare",
                  choices = (df  %>%
                                filterPlayers(input$ageRange,
                                              input$valueRange,
                                              input$position,
                                              input$nationality,
                                              input$league,
                                              input$club) %>%
                    nearest(searchPlayerCompare(),weights(),error=FALSE))[,2],
                  selected = c())
    })
    
    output$radarMeCompare <- renderUI({
      selectInput(inputId = "radarFindMe",
                  label = "Compare",
                  choices = (df  %>%
                               filterPlayers(input$ageRange,
                                             input$valueRange,
                                             input$position,
                                             input$nationality,
                                             input$league,
                                             input$club) %>%
                               nearest(myPlayerCompare(),weights(),error=FALSE))[,2],
                  selected = c())
    })
    
    radarSearchCompare <- reactive({df[df$full_name == input$radarSearchCompare,]})
    radarFindMe <- reactive({df[df$full_name == input$radarFindMe,]})
    
    output$radarSearch <- renderPlot({
      # Create data: note in High school for Jonathan:
      data=as.data.frame(((searchPlayerCompare())))[,30:63]
      data = rbind(data,(radarSearchCompare())[,30:63])
      # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
      data=rbind(rep(99,1) , rep(0,10) , data)
      
      # Custom the radarChart !
      radarchart( data  , axistype=1 ,
                  
                  #custom polygon
                  pcol=c(rgb(0.2,0.5,0.5,0.8),rgb(0.8,0.2,0.2,0.8)) , pfcol=c(rgb(0.2,0.5,0.5,0.4),rgb(0.8,0.2,0.2,0.4)) , plwd=4 ,
                  
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,99,25), cglwd=0.8,
                  
                  #custom labels
                  vlcex=0.65
      )
    })

  output$radarFindMe <- renderPlot({
    # Create data: note in High school for Jonathan:
    data = as.data.frame((myPlayerCompare()))[,30:63]
    data2 = radarFindMe()[,30:63]
    colnames(data2) = colnames(data)
    rownames(data2) = rownames(data)
    data = rbind(data,data2)
    colnames(data) = colnames(df[,30:63])
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    data=rbind(rep(99,1) , rep(0,10) , data)
    
    # Custom the radarChart !
    radarchart( data  , axistype=1 ,
                
                #custom polygon
                pcol=c(rgb(0.2,0.5,0.5,0.8),rgb(0.8,0.2,0.2,0.8)) , pfcol=c(rgb(0.2,0.5,0.5,0.4),rgb(0.8,0.2,0.2,0.4)) , plwd=4 ,
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,99,25), cglwd=0.8,
                
                #custom labels
                vlcex=0.65
    )
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

