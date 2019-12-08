################################################################################
# MSBA Data Visualization5
# November 2017
#
# HW5: Analysis with Shiny
#
# Your Name: Oliver Yuan
################################################################################
library(shiny)
library(tidyverse)
library(scales)
gfm <- read.csv("HW5_GoFundMe.csv")
gfm$Category <- as.factor(gfm$Category)
gfm$Success <- as.factor(gfm$Success)
gfm$Beneficiary <- as.factor(gfm$Beneficiary)
levels(gfm$Category) <- gsub("_", "\n", levels(gfm$Category))
se <- function(x){ sqrt(var(x, na.rm=T)/length(x))}
################################################################################
# UI function
ui <- fluidPage(
  titlePanel("HW 5: Go Fund Me Insights"),
  tabsetPanel(
    tabPanel("Explore",
             sidebarLayout(
               sidebarPanel(selectInput("xvar", "Categorical Variable (X-axis)", choices = names(gfm[12:14]), selected = "category"),
                            selectInput("yvar", "Numeric Variable (Y-axis)", choices = names(gfm[2:12]), selected = "AmountRaised"),
                            actionButton("go","Go")
                 # Question 1a: create the user inputs here.
                 
               ), 
               mainPanel(
                 plotOutput("distPlot")
               ))
    ), tabPanel("Insight",
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
    if (!class(gfm[,input$xvar])%in% c("factor")) { gfm[,input$xvar] <- as.factor(gfm[,input$xvar])}
    if (!class(gfm[,input$yvar])%in% c("numeric", "integer")) { gfm[,input$yvar] <- as.numeric(gfm[,input$yvar]) }
    cbind.data.frame(x=gfm[,input$xvar],y=gfm[,input$yvar])
  })
  # Question 1b: Create a reactive dataset for the plot
  plotData <- reactive({
    restrictedData() %>%  
      group_by(x) %>% 
      summarize(ymean = mean(y, na.rm = T), std = se(y)) %>% 
      arrange(desc(ymean)) %>% 
      head(10)
    })
    
  insightData <- reactive({
  gfm %>% 
      select(6,12) %>% 
      group_by(Category) %>% 
      summarise(ct = n()) %>% 
      arrange(desc(ct)) %>% 
      head(10)
      
    
  })

    

    

  # end Question 1b  
    observeEvent(input$go, {
    output$distPlot <- renderPlot({
      if (is.null(plotData())) {return(NULL)}
      # Question 1c: Create the categorical plot here.
      ggplot(plotData()) +
        geom_bar(mapping = aes(x = reorder(x, -ymean), y = ymean), stat = "identity", fill = "#FF6633")+
        geom_errorbar(mapping = aes(x = reorder(x, -ymean), ymin = ymean - std, ymax = ymean + std))+
        xlab(input$xvar)+
        ylab(input$yvar)+
        ggtitle("Explore the Data")
    })
  })
  output$insightPlot <- renderPlot({
    # Q2a: Insert the insight plot here.
    label.isx <- c("Competitions\nCampaigns","Extra\ncurriculars","Earthquakes",
                   "Breast\nCancer","Study\nAbroad","Mosque","Nationals","Miss\nAmerica","Bridal\nShower","Woodworking")
    ggplot(insightData()) +
      geom_bar(mapping = aes(x = reorder(Category, -ct), y = ct), stat = "identity", fill = "#99CCCC")+
      xlab("Category")+
      ylab("Count")+
      ggtitle(" The Most Top 10 Frequent Categories")

      })
  output$insights <- renderText({
    # Q2b: Insert the caption text here.
    "Study aboard is the most frequent category. Also, we can notice travel campaigns is the forth frequent reason. Both of these indicate that people are passionate about travelling and studying in other places. What's more, people are willing to spend money on sports and pets, which account for 5 out of 10 categories."
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

