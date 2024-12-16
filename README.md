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
climate_lamberton <- read_excel(wrangle, sheet = "Climate Lamberton")
climate_waseca <- read_excel(wrangle, sheet = "Climate Waseca")
#create weather data graphs lamberton ----
climate_lamberton$month <- as.factor(format(climate_lamberton$date, "%m"))
climate_lamberton$year <- as.factor(format(climate_lamberton$date, "%Y"))
str(climate_lamberton)
mean_temp_by_month_lamberton <- climate_lamberton %>%
  dplyr::group_by(year, month)%>%
  dplyr::summarise(
    mean_maxT = mean(maxT, na.rm = TRUE),
    mean_minT = mean(minT, na.rm = TRUE),
    mean_precip = mean(precipitation, na.rm = TRUE),
    .groups = "keep"
  )
ggplot(mean_temp_by_month_lamberton, aes(x = month, y = mean_maxT, fill = month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Average Maximum Temperature (°F)", title = "Monthly Average Maximum Temperatures Lamberton") +
  theme_minimal() +  # Base theme
  
  # Remove gridlines
  theme(
    # Bold and change font size of axis labels and title
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    
    # Bold the x-axis labels (months)
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    
    # Bold, change font size, and center the plot title
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Comma added here
    
    # Bold and change font size of the legend (for the colors)
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )
ggplot(mean_temp_by_month_lamberton, aes(x = month, y = mean_minT, fill = month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Average Minimum Temperature (°F)", title = "Monthly Average Minimum Temperatures Lamberton") +
  theme_minimal() +  # Base theme
  
  # Remove gridlines
  theme(
    # Bold and change font size of axis labels and title
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    
    # Bold the x-axis labels (months)
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    
    # Bold, change font size, and center the plot title
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Comma added here
    
    # Bold and change font size of the legend (for the colors)
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )
#precip data
ggplot(mean_temp_by_month_lamberton, aes(x = month, y = mean_precip, fill = month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Average Precipitation (in)", title = "Monthly Average Precipitation Lamberton") +
  theme_minimal() +  # Base theme
  
  # Remove gridlines
  theme(
    # Bold and change font size of axis labels and title
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    
    # Bold the x-axis labels (months)
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    
    # Bold, change font size, and center the plot title
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Comma added here
    
    # Bold and change font size of the legend (for the colors)
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )
#create weather data graphs waseca ----
climate_waseca <- climate_waseca %>% filter(!is.na(maxT))
climate_waseca$month <- as.factor(format(climate_waseca$date, "%m"))
climate_waseca$year <- as.factor(format(climate_waseca$date, "%Y"))
which(is.na(climate_waseca$maxT)) # had issue with an NA being in column so I couldn't average
climate_waseca$maxT[370] <- NA
climate_waseca$maxT <- as.numeric(climate_waseca$maxT)
climate_waseca$precipitation <- as.numeric(climate_waseca$precipitation)
str(climate_waseca)
mean_temp_by_month_waseca <- climate_waseca %>%
  dplyr::group_by(year, month)%>%
  dplyr::summarise(
    mean_maxT = mean(maxT, na.rm = TRUE),
    mean_minT = mean(MinT, na.rm = TRUE),
    mean_precip = mean(precipitation, na.rm = TRUE),
    .groups = "keep"
  )
ggplot(mean_temp_by_month_waseca , aes(x = month, y = mean_maxT, fill = month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Average Maximum Temperature (°F)", title = "Monthly Average Maximum Temperatures Waseca") +
  theme_minimal() +  # Base theme
  
  # Remove gridlines
  theme(
    # Bold and change font size of axis labels and title
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    
    # Bold the x-axis labels (months)
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    
    # Bold, change font size, and center the plot title
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Comma added here
    
    # Bold and change font size of the legend (for the colors)
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )
ggplot(mean_temp_by_month_waseca , aes(x = month, y = mean_minT, fill = month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Average Minimum Temperature (°F)", title = "Monthly Average Minimum Temperatures Waseca") +
  theme_minimal() +  # Base theme
  
  # Remove gridlines
  theme(
    # Bold and change font size of axis labels and title
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    
    # Bold the x-axis labels (months)
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    
    # Bold, change font size, and center the plot title
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Comma added here
    
    # Bold and change font size of the legend (for the colors)
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )
#precip data
ggplot(mean_temp_by_month_waseca, aes(x = month, y = mean_precip, fill = month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Average Precipitation (in)", title = "Monthly Average Precipitation Waseca") +
  theme_minimal() +  # Base theme
  
  # Remove gridlines
  theme(
    # Bold and change font size of axis labels and title
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    
    # Bold the x-axis labels (months)
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    
    # Bold, change font size, and center the plot title
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Comma added here
    
    # Bold and change font size of the legend (for the colors)
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )

#Seperating Climate Data into Years----
#%m/%d/%Y
#lamberton
climate_lamberton$date<-as.Date(climate_lamberton$date, format = "%m/%d/%Y")
#truncate data and cut out irrelevant years only need data for 2014
climate_lamberton2014<-climate_lamberton %>%
  filter(date>"2014-01-1"&date<"2014-12-31")
#truncate data and cut out irrelevant years only need data for 2015
climate_lamberton2015<-climate_lamberton %>%
  filter(date>"2015-01-1"&date<"2015-12-31")
#waseca
climate_waseca$date<-as.Date(climate_waseca$date, format = "%m/%d/%Y")
#truncate data and cut out irrelevant years only need data for 2014
climate_waseca2014<-climate_waseca %>%
  filter(date>"2014-01-1"&date<"2014-12-31")
#truncate data and cut out irrelevant years only need data for 2015
climate_waseca2015<-climate_waseca %>%
  filter(date>"2015-01-1"&date<"2015-12-31")
#have one NA in climate waseca 2015 and want to remove it
climate_waseca2015$maxT[4] <- 0

#adding gdd and four loops to all four climate sets ----
#establish b temp
b_temp<-40 #important columns are Day, Max, Min, GDD
#lamberton 2014
climate_lamberton2014 <- climate_lamberton2014 %>%
  mutate(gdd_minT=ifelse(minT<b_temp,b_temp,minT)) #trying to fix temp data for gdd measurements

climate_lamberton2014$avg_temp <- ((climate_lamberton2014$maxT+climate_lamberton2014$gdd_minT)/2)

climate_lamberton2014 <-climate_lamberton2014 %>%
  mutate(truegdd=ifelse((((climate_lamberton2014$maxT+climate_lamberton2014$gdd_minT)/2)-b_temp)<0,0,((climate_lamberton2014$maxT+climate_lamberton2014$gdd_minT)/2)-b_temp))

#time to learn four loops!----
#important columns are Day, Max, Min, GDD
climate_lamberton2014$c_gdd<-NA #create the column with nothing in it
climate_lamberton2014$c_gdd[1]<-climate_lamberton2014$truegdd[1] #fill the first row of the new column with what's in the d_gdd column in that first row
for(i in 2:length(climate_lamberton2014$c_gdd)){
  climate_lamberton2014$c_gdd[i]<-climate_lamberton2014$truegdd[i]+climate_lamberton2014$c_gdd[i-1]
}
#lamberton 2015
climate_lamberton2015 <- climate_lamberton2015 %>%
  mutate(gdd_minT=ifelse(minT<b_temp,b_temp,minT)) #trying to fix temp data for gdd measurements

climate_lamberton2015$avg_temp <- ((climate_lamberton2015$maxT+climate_lamberton2015$gdd_minT)/2)

climate_lamberton2015 <-climate_lamberton2015 %>%
  mutate(truegdd=ifelse((((climate_lamberton2015$maxT+climate_lamberton2015$gdd_minT)/2)-b_temp)<0,0,((climate_lamberton2015$maxT+climate_lamberton2015$gdd_minT)/2)-b_temp))

#time to learn four loops!----
#important columns are Day, Max, Min, GDD
climate_lamberton2015$c_gdd<-NA #create the column with nothing in it
climate_lamberton2015$c_gdd[1]<-climate_lamberton2015$truegdd[1] #fill the first row of the new column with what's in the d_gdd column in that first row
for(i in 2:length(climate_lamberton2015$c_gdd)){
  climate_lamberton2015$c_gdd[i]<-climate_lamberton2015$truegdd[i]+climate_lamberton2015$c_gdd[i-1]
}
#waseca 2014
climate_waseca2014 <- climate_waseca2014 %>%
  mutate(gdd_minT=ifelse(MinT<b_temp,b_temp,MinT)) #trying to fix temp data for gdd measurements

climate_waseca2014$avg_temp <- ((climate_waseca2014$maxT+climate_waseca2014$gdd_minT)/2)

climate_waseca2014 <-climate_waseca2014 %>%
  mutate(truegdd=ifelse((((climate_waseca2014$maxT+climate_waseca2014$gdd_minT)/2)-b_temp)<0,0,((climate_waseca2014$maxT+climate_waseca2014$gdd_minT)/2)-b_temp))

#time to learn four loops!----
#important columns are Day, Max, Min, GDD
climate_waseca2014$c_gdd<-NA #create the column with nothing in it
climate_waseca2014$c_gdd[1]<-climate_waseca2014$truegdd[1] #fill the first row of the new column with what's in the d_gdd column in that first row
for(i in 2:length(climate_waseca2014$c_gdd)){
  climate_waseca2014$c_gdd[i]<-climate_waseca2014$truegdd[i]+climate_waseca2014$c_gdd[i-1]
}
#waseca2015
climate_waseca2015 <- climate_waseca2015 %>%
  mutate(gdd_minT=ifelse(MinT<b_temp,b_temp,MinT)) #trying to fix temp data for gdd measurements

climate_waseca2015$avg_temp <- ((climate_waseca2015$maxT+climate_waseca2015$gdd_minT)/2)

climate_waseca2015 <-climate_waseca2015 %>%
  mutate(truegdd=ifelse((((climate_waseca2015$maxT+climate_waseca2015$gdd_minT)/2)-b_temp)<0,0,((climate_waseca2015$maxT+climate_waseca2015$gdd_minT)/2)-b_temp))

#time to learn four loops!----
#important columns are Day, Max, Min, GDD
climate_waseca2015$c_gdd<-NA #create the column with nothing in it
climate_waseca2015$c_gdd[1]<-climate_waseca2015$truegdd[1] #fill the first row of the new column with what's in the d_gdd column in that first row
for(i in 2:length(climate_waseca2015$c_gdd)){
  climate_waseca2015$c_gdd[i]<-climate_waseca2015$truegdd[i]+climate_waseca2015$c_gdd[i-1]
}
