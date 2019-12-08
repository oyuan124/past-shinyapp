crime_ge_type <- unique(clean_ver1$crime_type)
crime_ge_type <- as.data.frame(crime_ge_type)
crime_ge_type$crime_ge_type <- gsub(pattern = ".*ATTEMPTED.*|.*ATTEMPT.*", 
                                    replacement = "Inchoate Crime", crime_ge_type$crime_ge_type)

crime_ge_type$crime_ge_type <- gsub(pattern = ".*BATTERY.*|.*SEX.*|.*ASSAULT.*|.*HOMICIDE.*", 
                                    replacement = "Personal Crime", crime_ge_type$crime_ge_type)

crime_ge_type$crime_ge_type <- gsub(pattern = ".*THEFT.*|.*ROBBERY.*|.*BURGLARY.*|.*STOLEN.*", 
                                    replacement = "Property Crime", crime_ge_type$crime_ge_type)

crime_ge_type$crime_ge_type <- gsub(pattern = ".*BOMB.*|.*WEAPON.*|.*SHOTS.*", 
                                    replacement = "Weapon Use", crime_ge_type$crime_ge_type)

crime_ge_type$crime_ge_type <- gsub(pattern = "TRAFFIC",
                                    replacement = "Traffic", crime_ge_type$crime_ge_type)


crime_ge_type$crime_ge_type[!grepl(pattern = "Crime|Traffic|Use",x = crime_ge_type$crime_ge_type)] <- "Non-violent Crime"


crime_ge_type1 <- table(crime_ge_type)
crime_ge_type1 <- as.data.frame(crime_ge_type1)


table(clean_ver1$year_occur, clean_ver1$crime_ge_type) %>% 
  as.data.frame() -> c1