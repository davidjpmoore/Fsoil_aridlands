# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we focus on the interactions Between Reco and GPP fluxes ###
# for all time, Pulse and non-Pulse time ##################################

# Open files with summary information
summary_P <- read.csv("data/Summary_eddy.csv")
USW_PulseN <- read.csv("data/USWPulseN.csv")
USW_Pulse <- read.csv("data/USWPulse.csv")

# Read other pulse division docs - with "DM" in their names
summary_P <- read.csv("data/years_sum1_DM.csv")
USW_PulseN <- read.csv("data/years_sum_Pulse0_DM.csv")
USW_Pulse <- read.csv("data/years_sum_Pulse1_DM.csv")


# Plot data For All time
summary_P %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('All time')+
  ylim(0,6)

# Plot data for pulse time
USW_Pulse %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Pulse time')+
  ylim(0,6)

# Plot data for non-Pulse time
USW_PulseN %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Non-pulse time')+
  ylim(0,6)



