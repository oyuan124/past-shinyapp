library(shiny)
library(leaflet)

name_of_area <- unique(clean_ver1$area_name)
ge_type <- unique(clean_ver1$crime_ge_type)
clean_ver1$lat <- as.numeric(clean_ver1$lat) 
clean_ver1$lng <- as.numeric(clean_ver1$lng) 


  ui = (
    fluidRow(
      titlePanel("Numbers and Geographical Distribution of Crime"),
      
      sidebarLayout(
        
        sidebarPanel(selectInput(inputId = "n_year", label = "Year", 
                                 choices = c(2012:2016), selected = 2012),
                     
                     selectInput(inputId = "n_area", label = "Division", 
                                 choices = name_of_area, selected = "Central"),
                     
                     selectInput(inputId = "n_getype", label = "Crime Type",
                                 choices = ge_type, selected = "Non-violent Crime")),
        
        mainPanel(tabsetPanel(
          tabPanel("Bar Chart", plotOutput("crimeplot")),
          tabPanel("Map", leafletOutput("mymap"))))
        
      )))
  
  
  server = function(input, output) {
    output$mymap <- renderLeaflet({ filter(clean_ver1, year_occur == input$n_year, area_name == input$n_area
                                           , crime_ge_type == input$n_getype) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(~lng, ~lat,
                   clusterOptions = markerClusterOptions())
      
    })
    
    output$crimeplot <- renderPlot({ filter(clean_ver1, year_occur == input$n_year, 
                                            area_name == input$n_area, crime_ge_type == input$n_getype) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = month_occur), fill = "#993300")+
        labs(title = "Crime trend in LA",
             subtitle = "Number of Crimes", 
             x = "Month", y = "Number of Crimes")
    })
  }
  
shinyApp(ui = ui, server = server)


#user change input
#output are plots   
#add elements as argument in fluidPage


#assemble inputs into outputs
#save objects to display to output$
#build objects useing render function
#use input values with input$
#{} code block