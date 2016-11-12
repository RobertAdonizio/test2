library(shiny)
library(ggplot2)
library(dplyr)
#CURRENT_DISPLAY_PRICE
#PRODUCT_CLASS_NAME
#PRODUCT_COUNTRY_ORIGIN_NAME 
#PRODUCT_ALCOHOL_PERCENT
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
    mainPanel(plotOutput("coolplot"),
              br(),br(),
              tableOutput("results"))
  )
)

server <- function(input, output) {
  output$coolplot <- renderPlot({
    filtered <-
      iris %>%
      filter(PetalLengthCm >= input$petalInput[1],
             PetalLengthCm <= input$petalInput[2],
             Species == input$typeInput
      )
    ggplot(filtered, aes(SepalLengthCm)) +
      geom_histogram()
      })
  
  output$results <- renderTable({
    filtered <-
      iris %>%
      filter(PetalLengthCm >= input$petalInput[1],
             PetalLengthCm <= input$petalInput[2],
             Species == input$typeInput
      )
    filtered
  })
}

shinyApp(ui = ui, server = server)

