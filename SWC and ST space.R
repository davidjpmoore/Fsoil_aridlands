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

# Open files with summary information
summary_P <- read.csv("data/Summary_eddy.csv")
USW_PulseN <- read.csv("data/USWPulseN.csv")
USW_Pulse <- read.csv("data/USWPulse.csv")


# First plot Reco for Pulse time + 15 cm depth
plot(USW_Pulse$meanSWC15, USW_Pulse$meanST15, 
     cex = USW_Pulse$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC (15 cm depth)", # add x-axis label
     ylab = "Mean soil temperature (15 cm depth)", # add y-axis label
     main = "SWC and Soil T - during pulses", # add plot title
     xlim=c(0,45))

# Plot Reco for non-pulse time + 15 cm depth
plot(USW_PulseN$meanSWC15, USW_PulseN$meanST15, 
     cex = USW_PulseN$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC (15 cm depth)", # add x-axis label
     ylab = "Mean soil temperature (15 cm depth)", # add y-axis label
     main = "SWC and Soil T - between pulses" , # add plot title
     xlim=c(0,45))

# Plot the same for the 5 cm depth
plot(USW_Pulse$meanSWC5, USW_Pulse$meanST5, 
     cex = USW_Pulse$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC (5 cm depth)", # add x-axis label
     ylab = "Mean soil temperature (5 cm depth)", # add y-axis label
     main = "SWC and Soil T - during pulses", # add plot title
     xlim=c(0,45))

plot(USW_PulseN$meanSWC5, USW_PulseN$meanST5, 
     cex = USW_PulseN$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC (5 cm depth)", # add x-axis label
     ylab = "Mean soil temperature (5 cm depth)", # add y-axis label
     main = "SWC and Soil T - between pulses" , # add plot title
     xlim=c(0,45))


# We need to choose the soil depth 

cor.test(summary_P$meanRECO, summary_P$meanSWC5)
#       cor 
# 0.4208387 
cor.test(summary_P$meanRECO, summary_P$meanSWC15)
#       cor 
# 0.1847413 

# For paper - let's choose 5 cm depth-graphs







