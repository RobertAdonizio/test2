library(shiny)
library(ggplot2)
library(dplyr)

iris <- read.csv("C:\\Users\\ADONIZIO_W\\Desktop\\LearningAppR\\learningappworking\\Iris.csv", stringsAsFactors = FALSE) 

ui <- fluidPage(
  titlePanel("Iris Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("petalInput", "PetalLengthCm", 0, 10, c(1, 4)),
      radioButtons("typeInput", "Species",
                   choices = c("Iris-setosa", "Iris-versicolor", "Iris-virginica"),
                   selected = "Iris-setosa")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$typeOutput <- renderUI({
    selectInput("typeInput", "Species",
                sort(unique(iris$Species)),
                selected = "Iris-setosa")
  })  
  
  filtered <- reactive({
    if (is.null(input$typeInput)) {
      return(NULL)
    }    
    
    iris %>%
      filter(PetalLengthCm >= input$petalInput[1],
             PetalLengthCm <= input$petalInput[2],
             Species == input$typeInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(SepalLengthCm)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)