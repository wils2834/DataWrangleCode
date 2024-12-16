# DataWrangleCode
Code for Data Wrangle Project in Agro 5605
library(readxl)
wrangle<-("C:\\Users\\swils\\Downloads\\03 Classes\\Agronomy Data\\DataWrangle\\Data Wrangle Sheet.xlsx")
lysimeter_2015 <- read_excel(wrangle, sheet = "Lysimeter-2015")
lysimeter_2014 <- read_excel(wrangle, sheet = "Lysimeter-2014")
soils_2014 <- read_excel(wrangle, sheet = "Soils-2014")
soils_2015 <- read_excel(wrangle, sheet = "Soils-2015")
moisture <- read_excel(wrangle, sheet = "Moisture")
yield <- read_excel(wrangle, sheet = "Yield")
climate_lamberton <- read_excel(wrangle, sheet = "Climate Lamberton")
climate_waseca <- read_excel(wrangle, sheet = "Climate Waseca")
library(googlesheets4)
library(lattice)
library(tidyverse)
library(plyr)
library(readr)
library(nlme)
library(emmeans)
library(multcomp)
library(multcompView)
str(soils_2014)
#take the data that have two runs and average them for soils 2015
soils_2015$`LOI OM` <- sapply(as.character(soils_2015$`LOI OM`), function(x) {
  if (grepl("/", x)) {
    # Split the string and calculate the average
    vals <- strsplit(x, " / ")[[1]]
    return((as.numeric(vals[1]) + as.numeric(vals[2])) / 2)
  } else {
    # Return the original value if there's no "/"
    return(as.numeric(x))
  }
})
soils_2015$ammonia <- sapply(as.character(soils_2015$ammonia), function(x) {
  if (grepl("/", x)) {
    # Split the string and calculate the average
    vals <- strsplit(x, " / ")[[1]]
    return((as.numeric(vals[1]) + as.numeric(vals[2])) / 2)
  } else {
    # Return the original value if there's no "/"
    return(as.numeric(x))
  }
})
soils_2015$nitrate <- sapply(as.character(soils_2015$nitrate), function(x) {
  if (grepl("/", x)) {
    # Split the string and calculate the average
    vals <- strsplit(x, " / ")[[1]]
    return((as.numeric(vals[1]) + as.numeric(vals[2])) / 2)
  } else {
    # Return the original value if there's no "/"
    return(as.numeric(x))
  }
})
soils_2015$TOC <- sapply(as.character(soils_2015$TOC), function(x) {
  if (grepl("/", x)) {
    # Split the string and calculate the average
    vals <- strsplit(x, " / ")[[1]]
    return((as.numeric(vals[1]) + as.numeric(vals[2])) / 2)
  } else {
    # Return the original value if there's no "/"
    return(as.numeric(x))
  }
})
#Take any measurements that are < 0.1 and make them 0
lysimeter_2015$nitrate <- sapply(as.character(lysimeter_2015$nitrate), function(x) {
  if (grepl("<", x)) {
    return(0)  # Replace with NA if '<' is found
  } else {
    return(x)  # Keep the original value
  }
})
#Take any measurements that are T and make them 0
climate_waseca$precipitation <- sapply(as.character(climate_waseca$precipitation), function(x) {
  if (grepl("T", x)) {
    return(0)  # Replace with 0 if 'T' is found
  } else {
    return(x)  # Keep the original value
  }
})
climate_waseca$snow <- sapply(as.character(climate_waseca$snow), function(x) {
  if (grepl("T", x)) {
    return(0)  # Replace with 0 if 'T' is found
  } else {
    return(x)  # Keep the original value
  }
})
climate_waseca$snow_depth <- sapply(as.character(climate_waseca$snow_depth), function(x) {
  if (grepl("T", x)) {
    return(0)  # Replace with 0 if 'T' is found
  } else {
    return(x)  # Keep the original value
  }
})
#Take any measurements that are M and make them 0
climate_lamberton$snow_depth <- sapply(as.character(climate_lamberton$snow_depth), function(x) {
  if (grepl("M", x)) {
    return(0)  # Replace with 0 if 'T' is found
  } else {
    return(x)  # Keep the original value
  }
})
climate_lamberton$snow <- sapply(as.character(climate_lamberton$snow), function(x) {
  if (grepl("M", x)) {
    return(0)  # Replace with 0 if 'T' is found
  } else {
    return(x)  # Keep the original value
  }
})
