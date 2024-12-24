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

# Open file with all eddy-covariance data (file was created in "Read_eddy.R")
 
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



















