# 16-05-2024
# Anastasia Makhnykina

 library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(reshape2)

### In this file we will create two data frames for Pulse and Non-pulse time ###

# Open file with all eddy-covariance data 
USWkg12_20_summary <- read.csv("data/USWkg12_20_summary.csv", 
                               header=TRUE, na.strings="NaN", skip=0)

# Create new columns for heavy rains
USWkg12_20_summary$bigR <- as.numeric(USWkg12_20_summary$sum_R>5)
USWkg12_20_summary$bigRmm <- as.numeric(USWkg12_20_summary$bigR)*as.numeric(USWkg12_20_summary$sum_R)

# Create DOY - column
USWkg12_20_summary$DOY <- paste(yday(USWkg12_20_summary$date))
USWkg12_20_summary$DOY <- as.numeric(as.character(USWkg12_20_summary$DOY))

# Adding Seasons of the year
USWkg12_20_summary$Season = vector(mode = 'character', length = nrow(USWkg12_20_summary))
USWkg12_20_summary$Season[USWkg12_20_summary$DOY %in% c(1:59,305:366)] = 'Winter'
USWkg12_20_summary$Season[USWkg12_20_summary$DOY %in% 60:181] = 'Spring'
USWkg12_20_summary$Season[USWkg12_20_summary$DOY %in% 182:304] = 'Summer'

# Create year - column
USWkg12_20_summary$year <- substr(USWkg12_20_summary$date, 1,4)
USWkg12_20_summary$year <- as.numeric(as.character(USWkg12_20_summary$year))

# Create Pulse_DOY - the DOY when rain > 5 mm happen
USWkg12_20_summary$DOY <- as.numeric(as.character(USWkg12_20_summary$DOY))
USWkg12_20_summary$Pulse_DOY <- USWkg12_20_summary$DOY*USWkg12_20_summary$bigR

# Make df with just Rains > 5 mm 
USW1220_Pulse <- USWkg12_20_summary %>%
  filter(bigRmm > 0)


#################### 5-days duration Pulse definition ########################
#######################################################################

# Make additional "test" df to make Pulse duration (5 days)
USW1220_Pulse$DOY <- as.numeric(as.character(USW1220_Pulse$DOY))

colnames(USWkg12_20_summary) [1] <- 'date'
test <- data.frame(DOY=USW1220_Pulse$date)
test$day1<-as.Date(test$DOY)+1
test$day2<-as.Date(test$DOY)+2
test$day3<-as.Date(test$DOY)+3
test$day4<-as.Date(test$DOY)+4
test$Sday <- yday(test$DOY)
test$DOY <- as.Date(test$DOY)

test2<- melt(test,id='Sday')

test3 <- test2 %>% 
  arrange(value)

test4 <- data.frame(unique(test2$value))

test4$date <- test4$unique.test2.value.
test5 <- test4 %>% 
  arrange(date)

test5$DOY <- yday(test5$date)
test5 <- test5[-c(1)]

test5$date <- as.Date(test5$date)
USWkg12_20_summary$date <- as.Date(USWkg12_20_summary$date)

summary_P <- merge(USWkg12_20_summary,test5,by="date",all.x=TRUE)
 
# Add PulseDays - column
summary_P$PulseDays <- summary_P$DOY.y

summary_P$PulseDays[is.na(summary_P$PulseDays)] <- 0

write.csv(summary_P, "data/Summary_eddy.csv")

# Create df for Pulse and Non-pulse time
USW_Pulse <- summary_P %>%
  filter(PulseDays > 0)

write.csv(USW_Pulse, "data/USWPulse.csv")


USW_PulseN <- summary_P %>%
  filter(PulseDays == 0)

write.csv(USW_PulseN, "data/USWPulseN.csv")


###################### DM Pulse Definition #####################################
################################################################################

# Open file with all eddy-covariance data 
USWkg12_20_summary <- read.csv("data/USWkg12_20_summary.csv", 
                               header=TRUE, na.strings="NaN", skip=0)
USWkg12_20_summary$date <- as.Date(USWkg12_20_summary$date)

# sort data by date
years_sum1 <- USWkg12_20_summary %>% arrange(date)

# Initialize the pulseduration_S variable as a vector of 0's
years_sum1$pulseduration_S <- rep(0, nrow(years_sum1))

# Find the indices of rows where there is a rain event with sum_R between 5 and 10
rain_indices_S <- which(years_sum1$sum_R > 5 & years_sum1$sum_R <= 10)

# Loop through each rain event
for (i in rain_indices_S) {
  # Set pulseduration_S to 8 for 8 days after the event
  years_sum1$pulseduration_S[i:(min(i+7, nrow(years_sum1)))] <- 8
}

# Create a new variable called pulseduration_M, initialized with zeros
years_sum1$pulseduration_M <- rep(0, nrow(years_sum1))

# Loop through each element of sum_R
for (i in 1:nrow(years_sum1)) {
  
  # Check if the sum_R value is between 10 and 20
  if (years_sum1$sum_R[i] > 10 & years_sum1$sum_R[i] <= 20) {
    
    # If yes, set the pulseduration_M variable to 14 for the next 14 days
    for (j in 1:14) {
      if (i + j <= nrow(years_sum1)) { # check if within range
        years_sum1$pulseduration_M[i + j] <- 14
      }
    }
    
  }
  
}

# Create a new variable called pulseduration_L, initialized with zeros
years_sum1$pulseduration_L <- rep(0, nrow(years_sum1))

# Loop through each element of sum_R
for (i in 21:nrow(years_sum1)) {
  
  # Check if the sum_R value was less than or equal to 20 in the previous day, and greater than 20 in the current day
  if (years_sum1$sum_R[i-1] <= 20 & years_sum1$sum_R[i] > 20) {
    
    # If yes, set the pulseduration_L variable to 20 for the next 20 days
    for (j in 0:19) {
      if (i + j <= nrow(years_sum1)) { # check if within range
        years_sum1$pulseduration_L[i + j] <- 20
      }
    }
    
  }
  
}

# create a new variable called max_pulse_duration that is the maximum of pulseduration_S, pulseduration_M, and pulseduration_L
years_sum1$max_pulse_duration <- pmax(years_sum1$pulseduration_S, years_sum1$pulseduration_M, years_sum1$pulseduration_L)

# sort data by date
years_sum1 <- years_sum1 %>% arrange(date)

# Identify days with rain events
years_sum1$rain_event <- ifelse(years_sum1$sum_R > 5, 1, 0)

# Create a new column in the dataframe to store days since last rain event
years_sum1$days_since_rain_event <- 0

# Loop through each row of the data
for (i in 2:nrow(years_sum1)) {
  # If sum_R is greater than 5, set days_since_rain_event to 0
  if (years_sum1$sum_R[i] > 5) {
    years_sum1$days_since_rain_event[i] <- 0
  } else {
    # If sum_R is less than or equal to 5, increment the days_since_rain_event by 1
    years_sum1$days_since_rain_event[i] <- years_sum1$days_since_rain_event[i-1] + 1
  }
}


# Create years_sum_Pulse0 df
years_sum_Pulse0 <- years_sum1 %>%
  filter(days_since_rain_event >= max_pulse_duration)


# Create years_sum_Pulse1 df
years_sum_Pulse1 <- years_sum1 %>%
  filter(days_since_rain_event < max_pulse_duration)

# Total rain for Pulse and non-pulse time
hist(subset(years_sum_Pulse0$sum_R, years_sum_Pulse0$sum_R != 0), 
     breaks = seq(0, 60, length.out = 30),
     main = "Non-pulse time", 
     xlab = "Rainfall total", 
     ylab = "Frequency",
     col = "red", 
     border = "white",
     lty = "solid",
     ylim = c(0, 100))

hist(subset(years_sum_Pulse1$sum_R, years_sum_Pulse1$sum_R != 0), 
     breaks = seq(min(years_sum_Pulse1$sum_R), max(years_sum_Pulse1$sum_R), length.out = 30),
     main = "Pulse time", 
     xlab = "Rainfall total", 
     ylab = "Frequency",
     col = "cyan", 
     border = "white",
     lty = "solid",
     ylim = c(0, 100))

# WRITE OUT NEW FILES
write_csv(years_sum_Pulse0, "data/years_sum_Pulse0_DM.csv")
write_csv(years_sum_Pulse1, "data/years_sum_Pulse1_DM.csv")
write_csv(years_sum1, "data/years_sum1_DM.csv")



















