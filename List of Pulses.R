#05/02/2022
#Anastasia Makhnykina
#grab data we need


#Load Datarain
datarain_pro=read.csv("data/datarain_processed.csv", header=TRUE, na.strings = "NaN")
#remove unnessessary columns


#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)
library(PerformanceAnalytics)
library(xtable)
library(Hmisc)
library(ggpubr)
library(ggplot2)
library(colorRamps)

#Create Pulses 2-18

Pulse1 <- 
  datarain %>% 
  filter(DOY_S %in% (7:28)) 
  
# Make subset of 26 columns

Pulse1 <- 
  datarain %>% 
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (7:28))
  
  

Pulse2 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (8:29)) 

Pulse3 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (14:39)) 

Pulse4 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (165:186)) 

Pulse6 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (177:198)) 

Pulse7 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (183:204)) 

Pulse8 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (186:207)) 

Pulse9 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (187:208)) 

Pulse10 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (191:212)) 

Pulse11 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (200:221)) 

Pulse12 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (201:222)) 

Pulse13 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (202:223)) 

Pulse14 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (206:227)) 

Pulse15 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (207:228)) 

Pulse16 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (215:236)) 

Pulse17 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (217:238)) 

Pulse18 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (344:365)) 


#Make summary for all 18 Pulses
Pulse1_sum <- Pulse1 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse2_sum <- Pulse2 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse3_sum <- Pulse3 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse4_sum <- Pulse4 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse6_sum <- Pulse6 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse7_sum <- Pulse7 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse8_sum <- Pulse8 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse9_sum <- Pulse9 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse10_sum <- Pulse10 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse11_sum <- Pulse11 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse12_sum <- Pulse12 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse13_sum <- Pulse13 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse14_sum <- Pulse14 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse15_sum <- Pulse15 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse16_sum <- Pulse16 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse17_sum <- Pulse17 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))

Pulse18_sum <- Pulse18 %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                    meanAT6=mean(AT6, na.rm=TRUE),
                    sum_R=mean(sum_rain, na.rm=TRUE),
                    rain_events=sum(RainEvent, na.rm=TRUE),
                    meanRH2=mean(RH2, na.rm=TRUE),
                    meanRH6=mean(RH6,na.rm=TRUE),
                    meanSWC5=mean(SWC5, na.rm=TRUE),
                    meanSWC15=mean(SWC15, na.rm=TRUE),
                    meanSWC30=mean(SWC30, na.rm=TRUE), 
                    meanST5=mean(ST5, na.rm=TRUE),
                    meanST15=mean(ST15, na.rm=TRUE),
                    meanST30=mean(ST30, na.rm=TRUE),
                    meanNEE=mean(NEE, na.rm=TRUE), 
                    meanGPP=mean(GPP, na.rm=TRUE),
                    meanRECO=mean(RECO, na.rm=TRUE))


# Make a Huuuge! summary table for the all 18 pulses = 16 variables + 22*18 rows should be Pulse(n)_sum files!!!









