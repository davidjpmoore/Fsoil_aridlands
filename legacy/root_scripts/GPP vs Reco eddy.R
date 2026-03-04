# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(ggpubr)

# In this file we focus on the interactions Between Reco and GPP fluxes ###
# for all time, Pulse and non-Pulse time ##################################


# Read pulse division docs
years_sum1 <- read.csv("data/years_sum1_DM.csv")
years_sum_Pulse0 <- read.csv("data/years_sum_Pulse0_DM.csv")
years_sum_Pulse1 <- read.csv("data/years_sum_Pulse1_DM.csv")


# Plot data For All time
years_sum1 %>%
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
years_sum_Pulse1 %>%
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
years_sum_Pulse0 %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Non-pulse time')+
  ylim(0,6)+
  xlim(0,6)



