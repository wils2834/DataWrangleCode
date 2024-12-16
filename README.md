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
lysimeter_2015 <- read_excel(wrangle, sheet = "Lysimeter-2015")
lysimeter_2014 <- read_excel(wrangle, sheet = "Lysimeter-2014")
#issue with nrate in lysimeter 2014
# Update the Nrate to 80 for the row where Nrate equals "N60"
lysimeter_2014$Nrate[lysimeter_2014$Nrate == "N60"] <- 80

#merge climate datasets climate and lysimeter 2014----
str(lysimeter_2014)
str(mean_temp_by_month_lamberton)
str(mean_temp_by_month_waseca)
names(lysimeter_2014)[which(names(lysimeter_2014)=="Location")]<-"location"
# Split the `date` column into year and month
lysimeter_2014_climate <- lysimeter_2014 %>%
  mutate(
    # Parse the date column as a proper date
    parsed_date = parse_date_time(date, orders = c("Y b", "Y B")), # Matches "2014 May" or "2014 September"
    year = year(parsed_date),          # Extract the year
    month = month(parsed_date)         # Extract the month (numeric)
  )
#add location to mean temp by month lamberton and waseca
mean_temp_by_month_lamberton <- mean_temp_by_month_lamberton %>%
  mutate(location = "Lam")
mean_temp_by_month_waseca <- mean_temp_by_month_waseca %>%
  mutate(location = "Was")
#merge the temp with lysimeter 2014
lysimeter_2014_climate <- lysimeter_2014
str(lysimeter_2014_climate)
str(mean_temp_by_month_lamberton) #monthly average of lamberton 2014
mclam14<-as.data.frame(mean_temp_by_month_lamberton) #force this into a dataframe
str(mclam14)
mclam14 <- mclam14 %>%
  mutate(
    year = as.numeric(year),    # Ensure year is numeric
    month = as.numeric(month)   # Ensure month is numeric
  )
str(mclam14)
str(mean_temp_by_month_waseca) #monthly averages of waseca 2014
mcwas14<-as.data.frame(mean_temp_by_month_waseca)
str(mcwas14)
mcwas14 <- mcwas14 %>%
  mutate(
    year = as.numeric(year),    # Ensure year is numeric
    month = as.numeric(month)   # Ensure month is numeric
  )
str(mcwas14)
mcall14<-rbind(mcwas14,mclam14) #merging both weather data sets together
str(mcall14)
str(lysimeter_2014_climate)
# i need to numerically change the mcall14 data to be 1 as 2014, 2 as 2015, and 3 as 2016 to match the structure of lysimeter 2014 climate
mcall14 <- mcall14 %>% # Update the 'year' column to match the actual calendar year
  mutate(year = case_when(
    year == 1 ~ 2014,
    year == 2 ~ 2015,
    year == 3 ~ 2016,
    TRUE ~ year # Keeps other values unchanged, if present
  ))
lysimeter_2014_climate <- lysimeter_2014 %>%
  left_join(mcall14, by = c("year", "month", "location")) #merging the weather with the lysimeter data

#merge climate datasets climate and lysimeter 2015----
str(lysimeter_2015)
str(climate_lamberton2015)
str(climate_waseca2015)
# turn date in lysimeter 2015 into actual date
#%m/%d/%Y
lysimeter_2015$date<-as.Date(lysimeter_2015$date, format = "%m/%d/%Y")
str(lysimeter_2015)
# turn date in climate waseca 2015 into actual date
#%m/%d/%Y
climate_waseca2015$date<-as.Date(climate_waseca2015$date, format = "%m/%d/%Y")
str(climate_waseca2015)
#merge the temp with lysimeter 2014
lysimeter_2015_climate <- lysimeter_2015
str(lysimeter_2015_climate)
str(climate_lamberton2015) 
str(climate_waseca2015)
#merging both weather data sets together 2015
# Standardize column names to ensure they match exactly
colnames(climate_waseca2015)[colnames(climate_waseca2015) == "MinT"] <- "minT"
# Bind the datasets together
mcall15 <- rbind(climate_lamberton2015, climate_waseca2015)
str(mcall15)
lysimeter_2015_climate <- lysimeter_2015_climate %>%
  left_join(mcall15, by = c("date", "location")) #merging the weather with the lysimeter data
#lysimeter and climate plots----
#2014
# Plot nitrate vs. precipitation grouped by Nrate
str(lysimeter_2014_climate)
xyplot(mean_precip ~ nitrate, data = lysimeter_2014_climate,
       groups = factor(Nrate),  # Group by Nrate
       type = c("p", "r"),      # Points and regression lines
       auto.key = list(columns = 3),  # Add a legend
       xlab = "Nitrate (mg/L)",       # x-axis label
       ylab = "Precipitation (in)",   # y-axis label
       main = "Nitrate vs Precipitation by Nrate 2014"
)
# Boxplot of nitrate by Nrate
bwplot(nitrate ~ factor(Nrate)| location, data = lysimeter_2014_climate,
       xlab = "Nrate (lbs/acre)",      # Label for x-axis
       ylab = "Nitrate (mg/L)",        # Label for y-axis
       main = "Nitrate Distribution by Nrate 2014"
)
#plot nitrate vs maxt temp by nrate 
xyplot(nitrate ~ mean_maxT, data = lysimeter_2014_climate,
       groups = factor(Nrate),   # Group by Nrate
       type = c("p", "r"),        # Points and regression lines
       auto.key = TRUE,           # Add a legend
       xlab = "Max Temperature (°C)", 
       ylab = "Nitrate (mg/L)",
       main = "Nitrate vs Max Temperature by Nrate 2014")
#2015
#nitrate numbers are whack in 2015 lets fix that 
# Clean and convert nitrate to numeric
lysimeter_2015_climate <- lysimeter_2015_climate %>%
  mutate(
    nitrate = as.numeric(gsub("[^0-9.]", "", nitrate))  # Remove non-numeric characters
  )
# Set small values to 0 and round to 2 decimal places
lysimeter_2015_climate <- lysimeter_2015_climate %>%
  mutate(
    nitrate = ifelse(nitrate < 0.01, 0, round(nitrate, 2))
  )
head(lysimeter_2015_climate$nitrate)

# Plot nitrate vs. precipitation grouped by Nrate
xyplot(precipitation ~ nitrate, data = lysimeter_2015_climate,
       groups = factor(Nrate),  # Group by Nrate
       type = c("p", "r"),      # Points and regression lines
       auto.key = list(columns = 3),  # Add a legend
       xlab = "Nitrate (mg/L)",       # x-axis label
       ylab = "Precipitation (in)",   # y-axis label
       main = "Nitrate vs Precipitation by Nrate 2015"
)

#plot nitrate vs maxt temp by nrate 
xyplot(nitrate ~ maxT, data = lysimeter_2015_climate,
       groups = factor(Nrate),   # Group by Nrate
       type = c("p", "r"),        # Points and regression lines
       auto.key = TRUE,           # Add a legend
       xlab = "Max Temperature (°C)", 
       ylab = "Nitrate (mg/L)",
       main = "Nitrate vs Max Temperature by Nrate 2015")
# Boxplot to visualize the distribution across months
bwplot(nitrate ~ factor(Nrate)| location, data = lysimeter_2015_climate,
       xlab = "Nrate (lbs/acre)",      # Label for x-axis
       ylab = "Nitrate (mg/L)",        # Label for y-axis
       main = "Nitrate Distribution by Nrate 2015"
)

lysimeter_2015_climate$Nrate_factor <- as.factor(lysimeter_2015_climate$Nrate)
lysimeter_2015_climate <- na.omit(lysimeter_2015_climate)
lysimeter_2015_climate$Nrate_factor <- as.factor(lysimeter_2015_climate$Nrate)
# Create the ggpairs plot, explicitly converting the relevant columns to numeric
lysimeter_2015_climate$Nrate_factor <- as.factor(lysimeter_2015_climate$Nrate)

ggpairs(lysimeter_2015_climate[, c("nitrate", "maxT", "minT", "precipitation", "Nrate")],
        aes(colour = lysimeter_2015_climate$Nrate_factor)) +  # Explicitly reference Nrate_factor
  theme_minimal()  # Optional: adds a clean theme to the plot
#Summarize soil moisture and lysimeter data by various time intervals (month, season, year)----
# Add 'season' function
get_season <- function(month) {
  case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5) ~ "Spring",
    month %in% c(6, 7, 8) ~ "Summer",
    month %in% c(9, 10, 11) ~ "Fall"
  )
}

# Prepare moisture dataset
moisture <- moisture %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    season = get_season(month)
  )

# Prepare lysimeter_2015 dataset
lysimeter_2015 <- lysimeter_2015 %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    season = get_season(month)
  )

# Prepare lysimeter_2014 dataset
lysimeter_2014 <- lysimeter_2014 %>%
  mutate(
    date = as.Date(parsed_date),
    season = get_season(month)
  )
# Summarize by month
moisture_summary_month <- moisture %>%
  group_by(year, month) %>%
  dplyr::summarise(
    mean_measurement = mean(measurement, na.rm = TRUE),
    sd_measurement = sd(measurement, na.rm = TRUE),
    count = n(),  # Add count of observations for context
    .groups = "drop"
  )
print(moisture_summary_month)
# Summarize by season
moisture_summary_season <- moisture %>%
  group_by(year, season) %>%
  dplyr::summarise(
    mean_measurement = mean(measurement, na.rm = TRUE),
    sd_measurement = sd(measurement, na.rm = TRUE),
    count = n(),  # Add count of observations for context
    .groups = "drop"
  )
print(moisture_summary_season)
# Summarize by year
moisture_summary_year <- moisture %>%
  group_by(location, year) %>%
  dplyr::summarize(
    mean_measurement = mean(measurement, na.rm = TRUE),
    sd_measurement = sd(measurement, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
print(moisture_summary_year)
# Convert Nrate in both datasets to numeric
lysimeter_2014 <- lysimeter_2014 %>%
  mutate(Nrate = as.numeric(Nrate))

lysimeter_2015 <- lysimeter_2015 %>%
  mutate(Nrate = as.numeric(Nrate))
# Convert Nrate in both datasets to numeric
lysimeter_2014 <- lysimeter_2014 %>%
  mutate(nitrate = as.numeric(nitrate))

lysimeter_2015 <- lysimeter_2015 %>%
  mutate(nitrate = as.numeric(nitrate))

# Combine lysimeter data for consistency
lysimeter_combined <- bind_rows(lysimeter_2014, lysimeter_2015)

# Summarize by month
lysimeter_summary_month <- lysimeter_combined %>%
  group_by(location, year, month) %>%
  dplyr::summarize(
    mean_nitrate = mean(nitrate, na.rm = TRUE),
    sd_nitrate = sd(nitrate, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
print(lysimeter_summary_month)
# Summarize by season
lysimeter_summary_season <- lysimeter_combined %>%
  group_by(location, year, season) %>%
  dplyr::summarize(
    mean_nitrate = mean(nitrate, na.rm = TRUE),
    sd_nitrate = sd(nitrate, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
print(lysimeter_summary_season)
# Summarize by year
lysimeter_summary_year <- lysimeter_combined %>%
  group_by(location, year) %>%
  dplyr::summarize(
    mean_nitrate = mean(nitrate, na.rm = TRUE),
    sd_nitrate = sd(nitrate, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
print(lysimeter_summary_year)
#lets do a regression and scatter plot featuring nitrate and precipitation----
# Scatter plot with regression lines
ggplot(lysimeter_2014_climate, aes(x = mean_precip, y = nitrate, color = Nrate)) +
  geom_point(size = 3, alpha = 0.6) +        # Scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines
  labs(
    title = "Nitrate vs. Mean Precipitation by Nrate (2014)",
    x = "Mean Precipitation (inches)",
    y = "Nitrate (mg/L)",
    color = "Nrate (lbs/acre)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
# Fit a regression model
nitrate_model <- lm(nitrate ~ mean_precip * Nrate, data = lysimeter_2014_climate)
# Display model summary
summary(nitrate_model)
#maybe temperature is a more indicative factor?
ggplot(lysimeter_2014_climate, aes(x = mean_maxT, y = nitrate, color = Nrate)) +
  geom_point(size = 3, alpha = 0.6) +        # Scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines
  labs(
    title = "Nitrate vs. Max Temp by Nrate (2014)",
    x = "Max Temp Precipitation (F)",
    y = "Nitrate (mg/L)",
    color = "Nrate (lbs/acre)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
# Fit a regression model
nitrate_model <- lm(nitrate ~ mean_maxT * Nrate, data = lysimeter_2014_climate)
# Display model summary
summary(nitrate_model)
#2015 now!
ggplot(lysimeter_2015_climate, aes(x = precipitation, y = nitrate, color = as.factor(Nrate))) +
  geom_point(size = 3, alpha = 0.6) +        # Scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines
  labs(
    title = "Nitrate vs. Precipitation by Nrate (2015)",
    x = "Precipitation (inches)",
    y = "Nitrate (mg/L)",
    color = "Nrate (lbs/acre)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
# Fit a regression model
nitrate_model_2015_precip <- lm(nitrate ~ precipitation * Nrate, data = lysimeter_2015_climate)
# Display model summary
summary(nitrate_model_2015_precip)
ggplot(lysimeter_2015_climate, aes(x = maxT, y = nitrate, color = as.factor(Nrate))) +
  geom_point(size = 3, alpha = 0.6) +        # Scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines
  labs(
    title = "Nitrate vs. Max Temperature by Nrate (2015)",
    x = "Max Temperature (F)",
    y = "Nitrate (mg/L)",
    color = "Nrate (lbs/acre)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Fit a regression model
nitrate_model_2015_temp <- lm(nitrate ~ maxT * Nrate, data = lysimeter_2015_climate)
# Display model summary
summary(nitrate_model_2015_temp)
yield <- read_excel(wrangle, sheet = "Yield")
str(yield)
# Convert columns to appropriate types
yield$year <- as.factor(yield$year)
yield$legume_inclusion <- as.factor(yield$legume_inclusion) # Convert to factor
yield$grain_yield <- as.numeric(as.character(yield$grain_yield)) #convert to numeric
yield$IWG_straw_yield <- as.numeric(as.character(yield$IWG_straw_yield)) #convert to numeric
yield$legume_biomass <- as.numeric(as.character(yield$legume_biomass)) #convert to numeric
#lets make plots with only yield data
bwplot(grain_yield ~ legume_inclusion | year, data = yield)
#lets make this better
library(lattice)
#lets make a box plot looking at legume inclusion and yield across two years
levels(yield$legume_inclusion) <- c("Legume Exclusion", "Legume Inclusion")
bwplot(
  grain_yield ~ legume_inclusion | year,
  data = yield,
  par.settings = list(
    box.rectangle = list(fill = c("lightblue", "lightcoral")),  # Fill colors for boxplots
    box.umbrella = list(col = "black"),  # Whiskers in black
    plot.symbol = list(col = "black")    # Median points in black
  ),
  xlab = "Legume Inclusion Status",
  ylab = "Grain Yield",
  main = "Grain Yield by Legume Inclusion and Year"
)
#t-test to see if grain yield differs significantly between legume inclusion
t_test_2014 <- t.test(subset(yield, year == 2014)$grain_yield ~ subset(yield, year == 2014)$legume_inclusion)
t_test_2015 <- t.test(subset(yield, year == 2015)$grain_yield ~ subset(yield, year == 2015)$legume_inclusion)
#See T-tests
t_test_2014
t_test_2015
#anova  to see if grain yield differs significantly between legume inclusion
anova_result <- aov(grain_yield ~ legume_inclusion, data = yield)
summary(anova_result)
#grain yield vs nitrate scatter
yield %>%
  ggplot(aes(x = Nrate, y = grain_yield,color = location)) +
  geom_point() +
  labs(title = "Grain Yield vs. Nitrogen Rate",
       x = "Nitrogen Rate (Nrate)", y = "Grain Yield")
#same but different
xyplot(grain_yield~Nrate, data=yield, auto.key = T, type=c("p","r"))
#density plot of grain yields
yield %>%
  ggplot(aes(x = grain_yield, fill = legume_inclusion)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year) +
  labs(title = "Density Plot of Grain Yield by Legume Inclusion",
       x = "Grain Yield", y = "Density") +
  scale_fill_manual(values = c("Legume Exclusion" = "lightblue", "Legume Inclusion" = "red"))

#regression model
lm_model <- lm(grain_yield ~ legume_inclusion + Nrate + year, data = yield)
summary(lm_model)

#lets add climate data----
climate_lambertonbothyear<-climate_lamberton %>%
  filter(date>"2014-01-1"&date<"2015-12-31")
climate_wasecabothyear<-climate_waseca %>%
  filter(date>"2014-01-1"&date<"2015-12-31")
climate_wasecabothyear$date<-as.Date(climate_wasecabothyear$date, format = "%m/%d/%Y")
str(climate_lambertonbothyear)
str(climate_wasecabothyear)
str(yield)
#lets join these by year and location
#merging both weather data sets together 2015
# Standardize column names to ensure they match exactly
colnames(climate_wasecabothyear)[colnames(climate_wasecabothyear) == "MinT"] <- "minT"
bothyearweatherlandw <- rbind(climate_lambertonbothyear, climate_wasecabothyear)
str(bothyearweatherlandw)
str(yield)
yieldandweather <- yield %>%
  left_join(bothyearweatherlandw, by = c("year", "location")) #merging the weather with the lysimeter data
#Plot yields by treatment averaged over different spatial and temporal scales 
str(yield)
str(yieldandweather)
# Plot the raw grain yield data without summarizing
ggplot(yieldandweather, aes(x = year, y = grain_yield, color = legume_inclusion)) +
  geom_point(aes(shape = legume_inclusion), size = 2, alpha = 0.6) +  # Points for each data point
  geom_line(aes(group = interaction(location, legume_inclusion)), alpha = 0.3) + # Optional: Lines to connect points by location and treatment
  facet_wrap(~location) +  # Separate plots for each location
  labs(
    title = "Grain Yield by Treatment for Each Site-Year Combination",
    x = "Year",
    y = "Grain Yield (kg/ha)",
    color = "Legume Inclusion Treatment",
    shape = "Legume Inclusion Treatment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    legend.position = "top" # Position the legend at the top
  )
#Yearly Average Yield by Treatment for Each Location
ggplot(yieldandweather, aes(x = year, y = grain_yield, color = legume_inclusion)) +
  stat_summary(fun = "mean", geom = "point", size = 3, position = position_dodge(width = 0.5)) +  # Show mean values
  stat_summary(fun = "mean", geom = "line", size = 1, position = position_dodge(width = 0.5)) +  # Line to connect mean values
  facet_wrap(~location) +  # Separate plots for each location
  labs(
    title = "Yearly Average Grain Yield by Treatment for Each Location",
    x = "Year",
    y = "Average Grain Yield (kg/ha)",
    color = "Legume Inclusion Treatment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


# Perform linear regression to check if environmental factors influence grain yield
lm_results <- lm(grain_yield ~ maxT + minT + precipitation, data = yieldandweather)
summary(lm_results)
#Plot proportion of biomass by tissue type----
# Convert data to long format for biomass types
biomass_long <- yield %>%
  dplyr::select(plot, legume_biomass, grain_yield, IWG_straw_yield) %>%
  pivot_longer(
    cols = c(legume_biomass, grain_yield, IWG_straw_yield),
    names_to = "tissue_type",
    values_to = "biomass"
  ) %>%
  group_by(plot) %>%
  mutate(proportion = biomass / sum(biomass, na.rm = TRUE)) %>%
  ungroup()
head(biomass_long)
# Stacked bar plot
ggplot(biomass_long, aes(x = factor(plot), y = proportion, fill = tissue_type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proportion of Biomass by Tissue Type for Each Plot",
    x = "Plot",
    y = "Proportion",
    fill = "Tissue Type"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Faceted plot
ggplot(biomass_long, aes(x = factor(plot), y = proportion, fill = tissue_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ tissue_type, scales = "free_y") +
  labs(
    title = "Proportion of Biomass by Tissue Type for Each Plot",
    x = "Plot",
    y = "Proportion",
    fill = "Tissue Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ANOVA and Linear regression
str(yieldandweather)
# Does grain yield differ significantly based on legume inclusion, N rate, and location?----
anova_model <- aov(grain_yield ~ legume_inclusion * Nrate * location, data = yieldandweather)
summary(anova_model)
# Violin Plot for ANOVA model
ggplot(yieldandweather, aes(x = legume_inclusion, y = grain_yield, fill = legume_inclusion)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  facet_wrap(~location) +
  labs(
    title = "Grain Yield Distribution by Legume Inclusion and Location",
    x = "Legume Inclusion",
    y = "Grain Yield",
    fill = "Legume Inclusion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# How do temperature (maxT, minT), precipitation, and legume inclusion influence grain yield?----
lm_model <- lm(grain_yield ~ maxT + minT + precipitation + legume_inclusion + Nrate, data = yieldandweather)
summary(lm_model)
soils_2014 <- read_excel(wrangle, sheet = "Soils-2014")
soils_2015 <- read_excel(wrangle, sheet = "Soils-2015")
moisture <- read_excel(wrangle, sheet = "Moisture")
#lets fix the dates and strange columns we don't need
str(soils_2014)
str(soils_2015)
soils_2014$date<-as.Date(soils_2014$Sample_date, format = "%m/%d/%Y")
soils_2015 <- soils_2015 %>%
  mutate(
    date_char = paste(Sample_date, "11-01"),  # Combine year with January 1
    parsed_date = ymd(date_char),             # Parse as date
    year = year(parsed_date)                  # Extract the year (optional, since you already have it)
  )
soils_2015 <- soils_2015 %>%
  dplyr::select(-c(date_char, parsed_date, Sample_date))

soils_2014$date <- soils_2014$`sampl_ date`
soils_2014 <- soils_2014 %>%
  dplyr::select(-c(11, `sampl_ date`))
#lets add location columns to soils 2015
# Add location based on plot number
soils_2015 <- soils_2015 %>%
  mutate(location = ifelse(substr(as.character(plot), 1, 1) == "1", "Was", 
                           ifelse(substr(as.character(plot), 1, 1) == "2", "Lam", NA)))

#lets make some graphs of just the soil data ----
# Convert columns to appropriate types
soils_2014$legume_inclusion <- as.factor(soils_2014$legume_inclusion) # Convert to factor
# Create a faceted plot by depth
plot_legume_moisture_depth <- ggplot(soils_2014, aes(x = legume_inclusion, y = Soil_moisture_gravimetric, color = legume_inclusion)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +  # Adds slight jitter for visibility
  geom_boxplot(alpha = 0.4, outlier.color = NA) +
  facet_wrap(~ depth) +  # Facet the plot by depth
  labs(
    title = "Effect of Legume Inclusion on Soil Moisture (by Depth) 2014",
    x = "Legume Inclusion (0 = No, 1 = Yes)",
    y = "Soil Moisture Gravimetric%"
  ) +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()
# Display the plot
print(plot_legume_moisture_depth)
ggplot(soils_2015, aes(x = depth, y = nitrate, fill = depth)) +
  geom_boxplot() +
  labs(title = "Nitrate Levels by Soil Depth 2015", x = "Soil Depth", y = "Nitrate (mg/L)") +
  theme_minimal()
ggplot(soils_2015, aes(x = TOC, y = nitrate)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Nitrate vs Total Organic Carbon 2015", x = "Total Organic Carbon (g/kg)", y = "Nitrate (mg/L)") +
  theme_minimal()
ggplot(soils_2014, aes(x = depth, y = nitrate, fill = depth)) +
  geom_boxplot() +
  labs(title = "Nitrate Levels by Soil Depth (2014)", x = "Soil Depth", y = "Nitrate (mg/L)") +
  theme_minimal()
ggplot(soils_2014, aes(x = legume_inclusion, y = nitrate, fill = legume_inclusion)) +
  geom_boxplot() +
  labs(title = "Nitrate Levels by Legume Inclusion (2014)", x = "Legume Inclusion", y = "Nitrate (mg/L)") +
  theme_minimal()
ggplot(soils_2014, aes(x = Nrate, y = nitrate)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Nitrate Levels by Nrate (2014)", x = "Nrate (lbs/acre)", y = "Nitrate (mg/L)") +
  theme_minimal()


#soil moisture ----
str(moisture)
moisture$date<-as.Date(moisture$date, format = "%m/%d/%Y")

#longtowide transformation attempt ----
#2014
soils_2014_wide <- soils_2014 %>%
  pivot_wider(
    names_from = depth,   # Use 'depth' column for new column names
    values_from = c(nitrate, ammonia, `LOI OM`, Soil_moisture_gravimetric),  # Columns we want to pivot
    values_fill = list(    # Fill missing values with 0 for each variable
      nitrate = 0, 
      ammonia = 0, 
      `LOI OM` = 0, 
      Soil_moisture_gravimetric = 0
    )
  )

# View the resulting wide format
head(soils_2014_wide)
#soils2015
soils_2015_wide <- soils_2015 %>%
  pivot_wider(
    names_from = depth,   # Use 'depth' column for new column names
    values_from = c('LOI OM', ammonia, nitrate, TOC),  # Columns to pivot
    values_fill = list(    # Fill missing values with 0 for each variable
      `LOI OM` = 0, 
      ammonia = 0, 
      nitrate = 0, 
      TOC = 0
    )
  )

# View the resulting wide format
head(soils_2015_wide)
str(soils_2014_wide)
str(soils_2015_wide)

#adding precipitation to soil moisture----
str(climate_lamberton)
str(climate_waseca)
#climate_waseca$date <- as.Date(climate_waseca$date, format = "%m/%d/%Y")
#names(climate_waseca)[names(climate_waseca) == "MinT"] <- "minT"# Rename 'MinT' to 'minT' to match 'climate_lamberton'
# Merge the datasets
weatherboth<-rbind(climate_lamberton,climate_waseca) #merging both weather data sets together
str(weatherboth)
str(moisture)
# Add precipitation lags to the moisture data
moisture_with_precip <- moisture %>%
  left_join(weatherboth, by = c("date", "location")) %>% # Join weather data
  arrange(plot, date) %>% # Ensure data is sorted by plot and date
  group_by(plot) %>% # Group by plot for lagging
  mutate(
    # Lagged precipitation for each location
    precip_1_day = lag(precipitation, 1),
    precip_2_day = lag(precipitation, 2),
    precip_4_day = lag(precipitation, 4),
    precip_6_day = lag(precipitation, 6)
  ) %>%
  ungroup()
str(moisture_with_precip)
#it worked! lets make some plots from this----
# Scatter plot of soil moisture vs. lagged precipitation (1 day)
ggplot(moisture_with_precip, aes(x = precip_1_day, y = measurement, color = location)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Soil Moisture vs. Precipitation",
    x = "Precipitation, and days before measurement [inches]",
    y = "Soil Moisture",
    color = "Location"
  ) +
  theme_minimal()
# second trial for this
# Convert data to long format for easier plotting
# Rename the existing 'precipitation' column to avoid conflict
moisture_with_precip <- dplyr::rename(moisture_with_precip, original_precipitation = precipitation)

# Convert data to long format for easier plotting
moisture_long <- moisture_with_precip %>%
  pivot_longer(
    cols = starts_with("precip_"),
    names_to = "precip_lag",
    values_to = "precipitation"
  )
# Create the scatter plot
ggplot(moisture_long, aes(x = precipitation, y = measurement)) +
  geom_point(alpha = 0.5, size = 2) +
  facet_wrap(~ precip_lag, scales = "free_x") +
  labs(
    title = "Soil Moisture vs Precipitation with Different Lags",
    x = "Precipitation (inches)",
    y = "Soil Moisture (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
# i dont love this visualization, probably wont include this
#boxplot of soil moisture by location
ggplot(moisture_with_precip, aes(x = as.factor(year), y = measurement, fill = location)) +
  geom_boxplot() +
  labs(
    title = "Soil Moisture by Location and Year",
    x = "Year",
    y = "Soil Moisture",
    fill = "Location"
  ) +
  theme_minimal()
#time series of soil moisture and precipitation
# Filter data for a specific plot
plot_data <- moisture_with_precip %>% filter(plot == 1105)
ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = measurement, color = "Soil Moisture"), size = 1) +
  geom_line(aes(y = precip_1_day, color = "Precipitation (1 Day Lag)"), linetype = "dashed", size = 1) +
  labs(
    title = "Soil Moisture and Precipitation Over Time (Plot 1105)",
    x = "Date",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Soil Moisture" = "blue", "Precipitation (1 Day Lag)" = "red"))

#Plot soil moisture by depth, location, year and difference through time.----
str(moisture)
str(moisture_with_precip)
# Step 1: Plot soil moisture by depth and location
ggplot(moisture_with_precip, aes(x = factor(depth), y = measurement, fill = location)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_wrap(~year) +
  labs(
    title = "Soil Moisture by Depth and Location",
    x = "Depth (cm)",
    y = "Soil Moisture",
    fill = "Location"
  ) +
  theme_minimal()
 # Step 2: Time series of soil moisture by location and depth
ggplot(moisture_with_precip, aes(x = date, y = measurement, color = as.factor(depth))) +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.7) +
  facet_wrap(~location) +
  labs(
    title = "Smoothed Trends of Soil Moisture Over Time by Depth and Location",
    x = "Date",
    y = "Soil Moisture",
    color = "Depth (cm)"
  ) +
  theme_minimal()
# Step 3: Comparison across years
ggplot(moisture_with_precip, aes(x = date, y = measurement, color = as.factor(depth))) +
  geom_line() +
  facet_grid(year ~ location) +
  labs(
    title = "Comparison of Soil Moisture Across Years",
    x = "Date",
    y = "Soil Moisture",
    color = "Depth (cm)"
  ) +
  theme_minimal()
#different box plot
ggplot(moisture_with_precip, aes(x = date, y = measurement, color = location)) +
  geom_line(alpha = 0.7) +
  facet_grid(depth ~ location)
  labs(
    title = "Soil Moisture Over Time by Depth and Location",
    x = "Date",
    y = "Soil Moisture",
    color = "Location"
  ) +
  theme_minimal()
#lets make another anova for this data set
str(soils_2014)
str(soils_2015)
str(moisture)
moisture$depth <- as.factor(moisture$depth)
moisture <- moisture %>%
  mutate(depth = case_when(
    depth == 50 ~ "0-6\"",
    depth == 100 ~ "6-18\"",
    # Add other mappings if needed
    TRUE ~ as.character(depth)  # Fallback to avoid NA
  ))
# Calculate mean measurement for each plot, depth, and year
mean_measurement <- moisture %>%
  group_by(plot, depth, year) %>%
  dplyr:: summarise(mean_measurement = mean(measurement, na.rm = TRUE),
                    count = n(),
                    .groups = "drop")

# View the resulting summary
head(mean_measurement)
#lets merge moisture and soils 2015 to make an anova to see if moisture impacts TOC significantly
soils_2015_moisture <- soils_2015 %>%
  left_join(mean_measurement, by = c("plot", "depth", "year"))
str(soils_2015_moisture)
glimpse(soils_2015_moisture)
#anova time!
# Ensure TOC and measurement columns are numeric
soils_2015_moisture <- soils_2015_moisture %>%
  mutate(
    TOC = as.numeric(TOC),
    measurement = as.numeric(mean_measurement)
  )
# Remove rows with missing values
anova_data <- soils_2015_moisture %>%
  filter(!is.na(TOC), !is.na(mean_measurement))
# Perform ANOVA
anova_result <- aov(TOC ~ measurement, data = anova_data)
# Display the summary
summary(anova_result)
ggplot(anova_data, aes(x = measurement, y = TOC, color = depth)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(
    title = "Soil Moisture Over Time by Depth and Location", 
    x = "Moisture", 
    y = "TOC", 
    color = "Depth (cm)"
  )

#lets merge moisture and soils 2014 to make an anova to see if moisture impacts LOI OM significantly
str(soils_2014)
str(moisture)
soils_2014$plot <- soils_2014$Plot
soils_2014_moisture <- soils_2014 %>%
  left_join(mean_measurement, by = c("plot", "depth", "year"))
str(soils_2014_moisture)
# Perform ANOVA 
anova_result <- aov(`LOI OM` ~ Soil_moisture_gravimetric, data = soils_2014_moisture_clean)
# Display the summary of the ANOVA results
summary(anova_result)
# Scatter plot with regression line
ggplot(soils_2014_moisture_clean, aes(x = Soil_moisture_gravimetric, y = `LOI OM`)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Relationship Between Soil Moisture and LOI OM",
    x = "Soil Moisture Gravimetric (%)",
    y = "Loss on Ignition Organic Matter (%)"
  ) +
  theme_minimal()
# yield added?
str(yield)
str(soils_2014_moisture)
soils_2014_moisturemean$year <- as.factor(soils_2014_moisturemean$year)
# Merge the datasets by plot and year
merged_yield_soil_moisture <- left_join(soils_2014_moisturemean, yield, by = c("plot", "year"))
# View the structure of the merged dataset
str(merged_yield_soil_moisture )
# Filter out rows with NA values in key variables
manova_data <- merged_yield_soil_moisture %>%
  filter(!is.na(mean_measurement), !is.na(`LOI OM`), !is.na(grain_yield))
# Perform MANOVA
manova_result <- manova(cbind(`LOI OM`, grain_yield) ~ mean_measurement, data = manova_data)
# Summarize MANOVA results
summary(manova_result)
# ANOVA for LOI OM
anova_loi <- aov(`LOI OM` ~ grain_yield, data = manova_data)
summary(anova_loi)
anova_OM <- aov(grain_yield ~ `LOI OM`, data = manova_data)
summary(anova_OM)
# ANOVA for Grain Yield
anova_grain <- aov(grain_yield ~ mean_measurement, data = manova_data)
summary(anova_grain)
# Grain Yield vs. Mean Soil Moisture
ggplot(manova_data, aes(x = mean_measurement, y = grain_yield)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Grain Yield vs. Mean Soil Moisture", x = "Mean Soil Moisture", y = "Grain Yield")

# LOI OM vs. Grain Yield
ggplot(manova_data, aes(x = grain_yield, y = `LOI OM`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "LOI OM vs. Grain Yield", x = "Grain Yield", y = "LOI OM")



# Grain Yield vs. Mean Soil Moisture
ggplot(manova_data, aes(x = grain_yield, y = mean_measurement)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Mean Soil Moisture vs Grain Yield", x = "Grain Yield", y = "Mean Soil Moisture")

# LOI OM vs. Grain Yield
ggplot(manova_data, aes(x = `LOI OM`, y = grain_yield)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Grain Yield vs LOI OM", x = "LOI OM", y = "Grain Yield")
