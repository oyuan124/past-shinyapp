#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(MASS)
library(caret)
library(e1071)
data(iris)
n = nrow(iris)
p = 0.8
train_index <- sample(1:n, floor(n*p), replace=FALSE)
iris_train <- iris[train_index,]
iris_test <- iris[-c(train_index),]
spe <- unique(iris_train$Species)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("K-Means For Iris Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("nk",
                     "Number of Clusters:",
                     min = 2,
                     max = 10,
                     value = 2)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   plotData <- reactive(
   {
     iris_train.kmeans <- iris_train
     irisCluster <- kmeans(iris_train[,1:4], input$nk, nstart = 20)
     iris_train.kmeans$cluster <- as.factor(irisCluster$cluster)
     iris_train.kmeans
   })  
  
   output$distPlot <- renderPlot({
     ggplot() + theme_classic() +
       geom_point(data=plotData(), aes(x=Petal.Length, y=Petal.Width, color=Species, fill=cluster),shape=21, size = 3) +
       geom_point(data=plotData(), aes(x=Petal.Length, y=Petal.Width), alpha=0.2, shape=21, size = 3) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

