### **Glance at Data** 

##**You may have specific interests in different years or areas.**  
  
##  **Choose them!**
  
##```{r echo = FALSE, message = FALSE, warning = FALSE}

## read data can create input variable ##

name_of_area <- unique(clean_ver1$area_name)

shinyApp(
  
  ui = fluidPage(
    inputPanel(
      
      selectInput(inputId = "n_year", label = "Year", 
                  choices = c(2012:2016), selected = 2012),
      
      selectInput(inputId = "n_area", label = "Area", 
                  choices = name_of_area, selected = "Central"),
      
      plotOutput("crimeplot"))),
  
  server = function(input, output) {
    
    output$crimeplot <- renderPlot({ filter(clean_ver1, year_occur == input$n_year, area_name == input$n_area) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = month_occur))
    })
    
  }
  
)

```


list_of_packages <- c("readr", "ggplot2", "tidyr", "dplyr", "magrittr", "DT", "knitr", "shiny", "leaflet")

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new.packages)
