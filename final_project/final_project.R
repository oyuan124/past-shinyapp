## load packages ##

library(readr) ## read and save file
library(ggplot2) ## visualize data
library(tidyr) ## tidyr data
library(dplyr) ## transform data
library(magrittr) ## generate pipe operator
library(DT) ## present data in webpage
library(knitr)
library(shiny)
library(leaflet)

## read data file and rename columns so their names are in proper formats and make sense ##



rawdata <- read.csv("LA_crimes_2012_2016.csv", header = TRUE, col.names = c("date_reported", "case_number", "date_occur", "time_occur", 
                                                                            "area_number", "area_name", "RD", "crime_code", 
                                                                 "crime_type", "crime_status", "crime_status_des", 
                                                                 "street_loc", "cross_street", "lat_lon"))  

## separate mm/dd/yy to 3 columns so that I can do analysis based on different scales of time ##   
  
  separate(rawdata, date_reported, c("month_reported", "day_reported", 
                            "year_reported"), sep = "/") %>% 
  
  separate(date_occur, c("month_occur", "day_occur", 
                           "year_occur"), sep = "/") %>% 

## drop useless columns, delete rows that contain missing values and store data into rds. format ##  
    
  select(-c(4,9,11,12,14,17)) %>% 
  
  na.omit() %>% 
  
  write_rds("clean_data_ver11.rds")

## Cleaning punctuations, spaces and redundant words in variable "crime_type" ##

  clean_ver1 <- read_rds("clean_data_ver1.rds")
  
  clean_ver1$crime_type <- gsub("\\(.*|#| DR", "", 
                                clean_ver1$crime_type)
  
  clean_ver1$crime_type <- gsub(",| - | -|- ", "-", clean_ver1$crime_type)
  
  clean_ver1$crime_status_des <- gsub("UNK|Unknown", "unknown", clean_ver1$crime_status_des)
  
  clean_ver1$crime_ge_type <- clean_ver1$crime_type
  
  clean_ver1$crime_ge_type <- gsub(pattern = ".*ATTEMPTED.*|.*ATTEMPT.*", 
                                      replacement = "Inchoate Crime", clean_ver1$crime_ge_type)
  
  clean_ver1$crime_ge_type <- gsub(pattern = ".*BATTERY.*|.*SEX.*|.*ASSAULT.*|.*HOMICIDE.*", 
                                      replacement = "Personal Crime", clean_ver1$crime_ge_type)
  
  clean_ver1$crime_ge_type <- gsub(pattern = ".*THEFT.*|.*ROBBERY.*|.*BURGLARY.*|.*STOLEN.*", 
                                      replacement = "Property Crime", clean_ver1$crime_ge_type)
  
  clean_ver1$crime_ge_type <- gsub(pattern = ".*BOMB.*|.*WEAPON.*|.*SHOTS.*", 
                                      replacement = "Weapon Use", clean_ver1$crime_ge_type)
  
  clean_ver1$crime_ge_type <- gsub(pattern = "TRAFFIC",
                                      replacement = "Traffic", clean_ver1$crime_ge_type)
  
  clean_ver1$crime_ge_type[!grepl(pattern = "[A-Za-z] Crime|Traffic|Use",x = clean_ver1$crime_ge_type)] <- "Non-violent Crime"
  
  clean_ver1$lat_lon <- gsub(pattern = "\\(|\\)",
                                   replacement = "", clean_ver1$lat_lon)
  
  separate(clean_ver1, lat_lon, c("lat", "lng"), sep = ",") %>% 
    na.omit() %>% 
    filter(lat != "0.0") -> clean_ver1



## barchart for crimes in different areas ## 
  

    ggplot(clean_ver1) +
    geom_bar(mapping = aes(x = reorder(area_name, area_name, function(x)-length(x))), fill = "#33CCCC") +
    labs(title = "Dangerous Areas in LA",
         subtitle = "Number of Crimes in Different Divisions", 
         x = "Area", y = "Number of Crimes") +
    theme(axis.text.x = element_text(angle = -45, hjust = -0.05))+
    facet_wrap(~ year_occur, ncol = 5)+
   
    
## barchart for Top 15 types of crime ##

#table(clean_ver1$crime_type) %>% 
  #as.data.frame() %>% 
  #arrange(desc(Freq)) %>% 
  #head(15) -> crime_index

  ggplot(data = clean_ver1)+
  geom_bar(mapping = aes(x = reorder(area_name, area_name, function(x)-length(x)), fill = "#FFCC99")) +
  labs(title = "The Most Common Crime", subtitle = "Numbers of different types of crime",
       x = "Type of Crime", y = "Count")+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.002))

## linechart for crime in different years ##

table(clean_ver1$year_occur) %>% 
  as.data.frame() %>% 
  arrange(Var1) %>% 
  ggplot() +
  geom_line(mapping = aes(x = Var1, y = Freq, group = 1), 
            linetype = 1, color = "#006699") +
  labs(title = "Numbers of Crime in Different Years",
       x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 0)) +
  geom_point(aes(x = Var1, y = Freq), size = 4, color = "#FF9933")

## Barchart for crime status in different years ##

ggplot(data = clean_ver1)+
  geom_bar(mapping = aes(x = crime_ge_type, fill = crime_status_des), position = "fill") +
  labs(title = "Crime Status in Different Years",
       x = "Year", y = "Proporrtion of Status") +
  theme(axis.text.x = element_text(angle = 0))+
  facet_wrap(~ year_occur, nrow = 3)



table(clean_ver1$year_occur, clean_ver1$crime_ge_type) %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_line(mapping = aes(x = Var1, y = Freq, group = Var2, color = Var2))+
  labs(title = "Trend of Crime Types",
    subtitle = "Numbers of Crime Types in Different Years",
    x = "Year", y = "Count of Crime") +
  geom_point(aes(x = Var1, y = Freq, group = Var2), size = 2, color = "#006666")




##########################
#clean_ver1$lat <- as.numeric(clean_ver1$lat) 
#$lng <- as.numeric(clean_ver1$lng) 

#table

#leaflet(data = clean_ver1) %>% addTiles() %>%
  #addMarkers(~lng, ~lat, clusterOptions = markerClusterOptions())
