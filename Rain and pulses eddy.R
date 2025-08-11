# 16-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we need to make some analysis of data we observed ###
# and plot the main dependencies and interactions ##################

# Open two files for analysis 
USWkg12_20_summary <- read.csv("data/USWkg12_20_summary.csv", 
                               header=TRUE, na.strings="NaN", skip=0)

# First Plot frequency of Rain events > 5 mm 
USWkg12_20_summary$observation <- 1:nrow(USWkg12_20_summary)

USWkg12_20_summary$DOY <- paste(yday(USWkg12_20_summary$date))
USWkg12_20_summary$DOY <- as.numeric(as.character(USWkg12_20_summary$DOY))
USWkg12_20_summary$bigR <- as.numeric(USWkg12_20_summary$sum_R>5)
USWkg12_20_summary$bigRmm <- as.numeric(USWkg12_20_summary$bigR)*as.numeric(USWkg12_20_summary$sum_R)
USWkg12_20_summary$year <- substr(USWkg12_20_summary$date, 1,4)
USWkg12_20_summary$year <- as.numeric(as.character(USWkg12_20_summary$year))


USW9sum <- USWkg12_20_summary%>%
  filter(bigRmm > 0)

USW9sum %>%
  #group_by(DOY) %>%
  arrange(date) %>%
  mutate(diff = observation - lag(observation, default = first(observation)))

USW9sum$diff <- USW9sum$observation - lag(USW9sum$observation, 
                                          default = first(USW9sum$observation))

USW9sum %>%
  ggplot(aes(x=bigRmm))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Rain > 5 mm')+
  ylab('Frequency')+
  xlab('Rain, mm')+
  theme_gray()+
  theme(text = element_text(size = 12))

# Plot graph for Periods without any rain
USW9sum %>%
  ggplot(aes(x=diff))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Periods without rain')+
  ylab('Frequency')+
  xlab('Days')+
  theme_gray()+
  theme(text = element_text(size = 12))



# This is our Figure 1.


write.csv(USW9sum, file = "data/USW9sum.csv")
write.csv(USWkg12_20_summary, file = "data/USWkg12_20_summary.csv")


