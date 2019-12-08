name_of_area <- unique(clean_ver1$area_name)
ge_type <- unique(clean_ver1$crime_ge_type)
clean_ver1$lat <- as.numeric(clean_ver1$lat) 
clean_ver1$lng <- as.numeric(clean_ver1$lng) 

shinyApp(
  
  ui = fluidPage(
    inputPanel(
      
      selectInput(inputId = "n_year", label = "Year", 
                  choices = c(2012:2016), selected = 2012),
      
      selectInput(inputId = "n_area", label = "Area", 
                  choices = name_of_area, selected = "Central"),
      
      selectInput(inputId = "n_getype", label = "Crime Type",
                  choices = ge_type, selected = "Traffic"),
      
      leafletOutput("mymap"))),
  
  server = function(input, output) {
    
    output$mymap <- renderLeaflet({ filter(clean_ver1, year_occur == input$n_year, area_name == input$n_area
                                        , crime_ge_type == input$n_getype) %>% 
        leaflet() %>% 
        setView(lng = 34.0522, lat = 118.2437, zoom = 12) %>% 
        addTiles() %>% 
        addMarkers(
          clusterOptions = markerClusterOptions())
        
    })
    
  }
  
)