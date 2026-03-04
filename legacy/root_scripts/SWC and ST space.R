# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we plot the Reco in SWC-Soil temperature space ###
# for Pulse and non-Pulse time ##################################


# Read pulse division docs
years_sum1 <- read.csv("data/years_sum1_DM.csv")
years_sum_Pulse0 <- read.csv("data/years_sum_Pulse0_DM.csv")
years_sum_Pulse1 <- read.csv("data/years_sum_Pulse1_DM.csv")


# We need to choose the soil depth 

cor.test(summary_P$meanRECO, summary_P$meanSWC5)
#       cor 
# 0.4208387 
cor.test(summary_P$meanRECO, summary_P$meanSWC15)
#       cor 
# 0.1847413 

# For paper - let's choose 5 cm depth-graphs



# Plot the same for the 5 cm depth
plot(years_sum_Pulse1$meanSWC5, years_sum_Pulse1$meanST5, 
     cex = years_sum_Pulse1$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC 5 cm, %", # add x-axis label
     ylab = "Mean Tsoil 5 cm, °C", # add y-axis label
     main = "SWC and Soil T - during pulses", # add plot title
     xlim=c(0,45),
     ylim=c(0,40))

plot(years_sum_Pulse0$meanSWC5, years_sum_Pulse0$meanST5, 
     cex = years_sum_Pulse0$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC 5 cm, %", # add x-axis label
     ylab = "Mean Tsoil 5 cm, °C", # add y-axis label
     main = "SWC and Soil T - between pulses" , # add plot title
     xlim=c(0,45),
     ylim=c(0,40))










