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

#### In This file we take the figures for Fig1 and Fig 2a (max pulse size) ##########

# Open two files for analysis 
USWkg12_20_summary <- read.csv("data/USWkg12_20_summary.csv", 
                               header=TRUE, na.strings="NaN", skip=0)
summary_P <- read.csv("data/Summary_eddy.csv")

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


# In the next block we will create figures for pulses for 3 seasons ###
# Spring, Summer and Winter pulses ####################################


USW9sum$Season = vector(mode = 'character', length = nrow(USW9sum))
USW9sum$Season[USW9sum$DOY %in% c(1:59,305:366)] = 'Winter'
USW9sum$Season[USW9sum$DOY %in% 60:181] = 'Spring'
USW9sum$Season[USW9sum$DOY %in% 182:304] = 'Summer'

# Winter pulses
Pulse_Win <- USW9sum %>%
  filter(Season == 'Winter')

Pulse_Win$meanRECO <- as.numeric(as.character(Pulse_Win$meanRECO))

Pulse_Win %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Winter pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

Pulse2Win <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (348:365))

Pulse2Win$meanRECO <- as.numeric(as.character(Pulse2Win$meanRECO))
Pulse2Win$meanSWC5 <- as.numeric(as.character(Pulse2Win$meanSWC5))
Pulse2Win$sdReco <- as.numeric(as.character(Pulse2Win$sdReco))
Pulse2Win$sdSWC5 <- as.numeric(as.character(Pulse2Win$sdSWC5))

Pulse2Win %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanRECO),size=2)+
  geom_point(aes(y=meanSWC5/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+geom_errorbar(aes(ymin=meanRECO - sdReco, ymax= meanRECO + sdReco), width=.2,
                  position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC5/10 - sdSWC5/10, ymax= meanSWC5/10 + sdSWC5/10), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('DOY')+
  ggtitle('Winter pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))


# Spring pulses
Pulse_Spr <- USW9sum %>%
  filter(Season == 'Spring')

Pulse_Spr$meanRECO <- as.numeric(as.character(Pulse_Spr$meanRECO))

Pulse_Spr %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Spring pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

Pulse1Spr <- USWkg12_20_summary %>%
  filter(year == 2014) %>%
  filter(DOY %in% (57:74))

Pulse1Spr$meanRECO <- as.numeric(as.character(Pulse1Spr$meanRECO))
Pulse1Spr$meanSWC5 <- as.numeric(as.character(Pulse1Spr$meanSWC5))
Pulse1Spr$sdReco <- as.numeric(as.character(Pulse1Spr$sdReco))
Pulse1Spr$sdSWC5 <- as.numeric(as.character(Pulse1Spr$sdSWC5))

Pulse1Spr %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanRECO),size=2)+
  geom_point(aes(y=meanSWC5/20), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanRECO - sdReco, ymax= meanRECO + sdReco), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC5/20 - sdSWC5/20, ymax= meanSWC5/20 + sdSWC5/20), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('DOY')+
  ggtitle('Spring pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*20, 
                                          name="SWC 5 cm , %"))

# Summer pulses
Pulse_Sum <- USW9sum %>%
  filter(Season == 'Summer')

Pulse_Sum$meanRECO <- as.numeric(as.character(Pulse_Sum$meanRECO))

Pulse_Sum %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Summer pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))


Pulse1Sum <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (181:198))

Pulse1Sum$meanSWC5 <- as.numeric(as.character(Pulse1Sum$meanSWC5))
Pulse1Sum$meanRECO <- as.numeric(as.character(Pulse1Sum$meanRECO))
Pulse1Sum$sdReco <- as.numeric(as.character(Pulse1Sum$sdReco))
Pulse1Sum$sdSWC5 <- as.numeric(as.character(Pulse1Sum$sdSWC5))

Pulse1Sum %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanRECO),size=2)+
  geom_point(aes(y=meanSWC5/5), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanRECO - sdReco, ymax= meanRECO + sdReco), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC5/5 - sdSWC5/5, ymax= meanSWC5/5 + sdSWC5/5), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('DOY')+
  ggtitle('Summer pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*5, 
                                          name="SWC 5 cm , %"))






