# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(ggpubr)

# In this file we will plot figures to show interactions between Rsoil and GPP

# Open new- updated files with Dave's pulse definition
summary_Cham <- read.csv("data/All summary chamber.csv")
Pulse_Cham <- read.csv("data/Pulse sum chamber.csv")
NonPulse_Cham <- read.csv("data/NonPulse sum chamber.csv")


# Plot figure for all time
summary_Cham %>%
  ggplot(aes(x=meanGPP, y = meanRsoil))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Soil emission, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('All time')+
  ylim(0,6)

# Plot figure for Pulse time
Pulse_Cham %>%
  ggplot(aes(x=meanGPP, y = meanRsoil))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Soil emission, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Pulse time')+
  ylim(0,6)

# Plot figure for non-pulse time
NonPulse_Cham %>%
  ggplot(aes(x=meanGPP, y = meanRsoil))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Soil emission, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Non-Pulse time')+
  ylim(0,6)+
  xlim(0,6)
