library(shiny)
library(shinythemes)
library(igraph)

facebook <- read.table('107.edges')
facebook <- graph_from_data_frame(facebook)
E(facebook)$arrow.size = .025

source('adopt.R')

ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Adoption of Product"),
                sidebarLayout(
                  sidebarPanel(

                    selectInput(inputId = "type", label = strong("Adopter Sample Population"),
                                choices = c('Entire Population','Degree','Closeness','Betweenness','Authority.score'),
                                selected = 'Entire Population'),
                    conditionalPanel(condition = "input.type != 'Entire Population'",
                                     selectInput(inputId = "direction", label = strong("High/Low"),
                                                 choices = c('High','Low'),
                                                 selected = 'High')
                    ),
                    numericInput(inputId = "thresh", label = strong("Threshold q"),
                                .1,min = 0, max = .5,step=.05),
                    
                    numericInput(inputId = "nadopt", label = strong("Initial Adopters"),
                                 3,min = 2, max = 20,step=1),
                    numericInput(inputId = "size", label = strong("Portion of Entire Graph"),
                                 .2,min = .1, max = 1,step=.05),
                    numericInput(inputId = "rounds", label = strong("Rounds"),
                                 4,min = 2, max = 6,step=2),
                    actionButton('button','Go')
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "adoptionRounds", height = "600px"),
                    tags$a(href = "https://snap.stanford.edu/data/", "Data Source: Stanford", target = "_blank")
                  )
        )
)

server <- function(input, output) {
  
  output$adoptionRounds <- renderPlot({
    par(mfrow = c(2,input$rounds/2))
    input$Button
    adopt(graph = facebook,
          sampPop = input$type,
          sampPopType = input$direction,
          viewPlots = TRUE,
          initialAdopters = input$nadopt,
          q = input$thresh,
          nRounds = input$rounds,
          nSamples = 1,
          portionOfGraph = input$size)
  })
}

shinyApp(ui = ui, server = server)
