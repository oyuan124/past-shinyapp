################################################################################
# MSBA Data Visualization5
# November 2017
#
# HW5: Analysis with Shiny
#
# Your Name: Kathy Sun
################################################################################
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
gfm <- read.csv("HW5_GoFundMe.csv")
se <- function(x){ sqrt(var(x, na.rm=T)/length(x))}
################################################################################
# UI function
ui <- fluidPage(
  titlePanel("HW 5: Go Fund Me Insights"),
  tabsetPanel(
        tabPanel("Explore",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "xvar",
                   label = "Categorical Variable (X-axis)",
                   choices = c("Category", "Success", "Beneficiary"),
                   selected = "Category"),
                 selectInput(
                   inputId = "yvar",
                   label = "Numeric Variable (Y-axis)",
                   choices = c("Backers", "AmountRaised", "Faves", "Goal", "MeanHappiness",
                               "MeanFear", "MeanSurprise", "MeanSadness", "MeanNeutral", 
                               "MeanAnger"),
                   selected = "AmountRaised"),
                 actionButton("go", label = "Go")
               ), 
               mainPanel(
                 plotOutput("distPlot")
               ))), 
        tabPanel("Insight",
                plotOutput("insightPlot"),
                textOutput("insights"))
  )
)

################################################################################
# Server function: 
# Server logic required to make the plots
server <- function(input, output) {
  restrictedData <- eventReactive(input$go,{
    # This verifies that the user inputs are the right class.
    if (!class(gfm[,input$xvar])%in% c("factor", "character")) { return(NULL) }
    if (!class(gfm[,input$yvar])%in% c("numeric", "integer")) { return(NULL) }
    cbind.data.frame(x=gfm[,input$xvar], y=gfm[,input$yvar])
  })
  # Question 1b: Create a reactive dataset for the plot
  plotData <- reactive({
              restrictedData() %>%
                group_by(x) %>%
                summarise(mean = mean(y, na.rm = T), se = se(y)) %>%
                arrange(desc(mean)) %>%
                top_n(10)
  })
    
    
    
    # end Question 1b  
  observeEvent(input$go, {
    output$distPlot <- renderPlot({
        if (is.null(plotData())) {return(NULL)}
        
        # Question 1c: Create the categorical plot here.
        ggplot(data = plotData()) +        
        geom_bar( aes(x = reorder(x, -mean), y=mean),stat ="identity", color="white", fill="grey") +
        geom_errorbar(aes(x=x, ymin = mean - se, ymax = mean + se)) +
        theme_classic() +
        ggtitle("Exploring the data")
        
      })
    })
  
  
  output$insightPlot <- renderPlot({

    
  })
  output$insights <- renderText({
    # Q2b: Insert the caption text here.
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

