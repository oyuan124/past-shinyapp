library(shiny)
library(tidyverse)
library(scales)
gfm <- read.csv("HW5_GoFundMe.csv")
gfm$Category <- as.factor(gfm$Category)
gfm$Success <- as.factor(gfm$Success)
gfm$Beneficiary <- as.factor(gfm$Beneficiary)
levels(gfm$Category) <- gsub("_", "\n", levels(gfm$Category))
se <- function(x){ sqrt(var(x, na.rm=T)/length(x))}

  plotData <- 
    gfm %>%  
      group_by(Category) %>% 
      summarize(ymean = mean(AmountRaised, na.rm = T), std = se(AmountRaised)) %>% 
      arrange(desc(ymean)) %>% 
      head(10)
  