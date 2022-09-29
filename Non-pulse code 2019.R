# 19-07-2022
# Anastasia Makhnykina

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
library(reshape2)
library(zoo)

# upload 2018 fluxes and meteo
datarain19 =read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv", 
                     header=TRUE, na.strings = "NaN")

# Make all steps to clean the initial document
datarain19$year=substr(datarain19$TIMESTAMP_START,1,4)
datarain19$month=substr(datarain19$TIMESTAMP_START, 5,6)
datarain19$day=substr(datarain19$TIMESTAMP_START, 7,8)
datarain19$hour=substr(datarain19$TIMESTAMP_START,9,10)
datarain19$min=substr(datarain19$TIMESTAMP_START,11,12)

datarain19$year1=substr(datarain19$TIMESTAMP_END,1,4)
datarain19$month1=substr(datarain19$TIMESTAMP_END, 5,6)
datarain19$day1=substr(datarain19$TIMESTAMP_END, 7,8)
datarain19$hour1=substr(datarain19$TIMESTAMP_END,9,10)
datarain19$min1=substr(datarain19$TIMESTAMP_END,11,12)

datarain19 <-  rename(datarain19, r=P, 
                      SWC5=SWC_1_1_1,
                      SWC15=SWC_1_2_1,
                      SWC30=SWC_1_3_1,
                      ST5=TS_1_1_1,  
                      ST15=TS_1_2_1, 
                      ST30=TS_1_3_1,
                      AT2=TA_1_2_1,
                      AT6=TA_1_1_1, 
                      RH2=RH_1_2_1,
                      RH6=RH_1_1_1)

datarain19$dateStart <- paste(datarain19$year, datarain19$month, datarain19$day, sep="-")
datarain19$timeStart <- paste(datarain19$hour,datarain19$min, sep=":")
datarain19$dateEnd <- paste(datarain19$year1, datarain19$month1, datarain19$day1, sep="-")
datarain19$timeEnd <- paste(datarain19$hour1,datarain19$min1, sep=":")

datarain19$data_time_Start <- paste(datarain19$dateStart,datarain19$timeStart)
datarain19$data_time_End <- paste(datarain19$dateEnd,datarain19$timeEnd)

datarain19$data_time_Start = as.POSIXlt(datarain19$data_time_Start, format = "%Y-%m-%d %H:%M")
datarain19$data_time_End = as.POSIXlt(datarain19$data_time_End, format = "%Y-%m-%d %H:%M")

datarain19$high_precip <- datarain19$r>5
datarain19$high_precip <- as.numeric(datarain19$r>5)
datarain19$high_precip <- as.numeric(datarain19$r>5)*datarain19$r

datarain19$DOY_S <- paste(yday(datarain19$dateStart))
datarain19$DOY_E <- paste(yday(datarain19$dateEnd))

datarain19$RainEvent <- paste(datarain19$r>0)
datarain19$RainEvent <- as.numeric(datarain19$r>0)
datarain19$Rain_DOY <- as.numeric(datarain19$RainEvent)*as.numeric(datarain19$DOY_S)
sum(datarain19$RainEvent, na.rm=FALSE)

datarain19$highR_event <- datarain19$high_precip>5 
datarain19$highR_event <- as.numeric(as.logical(datarain19$highR_event))
sum(datarain19$highR_event) 

datarain19$Rain_DOY_high <- as.numeric(datarain19$highR_event)*as.numeric(datarain19$DOY_S)

Pulses_2019 <- datarain19 %>%
  filter(highR_event == 1)

# Calculate Pulse and Non-Pulse times fluxes
datarain19$DOY_S <- as.numeric(as.character(datarain19$DOY_S))

summary2019_all <- datarain19 %>%
  group_by(DOY_S) %>%
  na.omit() %>%
  dplyr :: summarise(meanAT2 = mean (replace(AT2, AT2 == -9999, NA),na.rm=TRUE),                     , 
                     meanAT6=mean(replace(AT6, AT6== -9999, NA),na.rm=TRUE),
                     sum_R=sum(r, na.rm=TRUE),
                     rain_events=sum(RainEvent, na.rm=TRUE),
                     meanRH2=mean(replace(RH2, RH2== -9999, NA),na.rm=TRUE),
                     meanRH6=mean(replace(RH6, RH6== -9999, NA),na.rm=TRUE),
                     meanSWC5=mean(replace(SWC5, SWC5== -9999, NA),na.rm=TRUE),
                     meanSWC15=mean(replace(SWC15, SWC15== -9999, NA),na.rm=TRUE),
                     meanSWC30=mean(replace(SWC30, SWC30== -9999, NA),na.rm=TRUE), 
                     meanST5=mean(replace(ST5, ST5== -9999, NA),na.rm=TRUE),
                     meanST15=mean(replace(ST15, ST15== -9999, NA),na.rm=TRUE),
                     meanST30=mean(replace(ST30, ST30== -9999, NA),na.rm=TRUE),
                     meanNEE=mean(NEE, na.rm=TRUE), 
                     meanGPP=mean(GPP, na.rm=TRUE),
                     meanRECO=mean(RECO, na.rm=TRUE), 
                     sdReco=sd(RECO, na.rm=TRUE))


summary2019_all$Season = vector(mode = 'character', length = nrow(summary2019_all))
summary2019_all$Season[summary2019_all$DOY_S %in% c(1:59,305:366)] = 'Winter'
summary2019_all$Season[summary2019_all$DOY_S %in% 60:181] = 'Spring'
summary2019_all$Season[summary2019_all$DOY_S %in% 182:304] = 'Summer'

summary2019_all$high_day <- paste(summary2019_all$sum_R>5)
summary2019_all$high_day <- as.numeric(summary2019_all$sum_R>5)

summary2019_all$Rain_DOY <- summary2019_all$high_day*summary2019_all$DOY_S

summary2019_all$Pulse_Days <- summary2019_all$high_day

Pulses_2019_n <- summary2019_all %>%
  filter(sum_R > 5)

colnames(summary2019_all)[1]<-'DOY'
test <- data.frame(DOY=Pulses_2019_n$DOY_S)
test$day1<-test$DOY+1
test$day2<-test$DOY+2
test$day3<-test$DOY+3
test$day4<-test$DOY+4
test2<-melt(test)


test2<-data.frame(DOY=unique(test2$value))
test2$Pulse_DOY <- test2$DOY


summary2019_new <- merge(summary2019_all,test2,by="DOY",all.x=TRUE)

######## Make table just with NA-Pulse_DOY

summary2019_new[is.na(summary2019_new)] = 0

summary2019_Pulse0 <- summary2019_new %>%
  filter(Pulse_DOY == 0) 

summary2019_Pulse1 <- summary2019_new %>%
  filter(Pulse_DOY >= 15) 



plot(summary2019_all$DOY)

summary2019_new %>%
  #filter(Pulse_Days ==1 ) %>%
  filter(Pulse_DOY == 0 ) %>%
  ggplot(aes(x=DOY, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  #stat_smooth(method = "lm", 
  #formula = y ~ poly(x, 2),size = 1)
  geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)+
  ggtitle("Reco 2019")

summary2019_new %>%
  #filter(Pulse_Days ==1 ) %>%
  filter(Pulse_DOY > 1 ) %>%
  ggplot(aes(x=DOY, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  #stat_smooth(method = "lm", 
  #formula = y ~ poly(x, 2),size = 1)
  geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)+
  ggtitle("Reco 2019")

summary2019_new %>%
  #filter(Pulse_Days ==1 ) %>%
  filter(Pulse_DOY >= 15 ) %>%
  ggplot(aes(x=meanST30, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  #geom_smooth()+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2),
              size = 1)+
  #geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)+
  ggtitle("Reco 2019")

summary2019_Pulse0 %>%
  ggplot(aes(x=meanSWC15, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  #stat_smooth(method = "lm", 
  #formula = y ~ poly(x, 2),size = 1)
  #geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)+
  ggtitle("Reco Pulse time")+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2),
              size = 1)



summary2019_Pulse0 %>%
  ggplot(aes(x=DOY, y = meanSWC30))+
  geom_point(size=2, shape = 1)+
  geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))

#stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
#stat_smooth(method = "lm", 
#formula = y ~ poly(x, 2),size = 1)
#geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)


summary2019_Pulse0 %>%
  ggplot(aes(x=meanSWC30, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2),size = 1
  )

############ Calculate r-coefficient and p-value for three relationships:
# 1 - Reco VS ST
# 2 - Reco VS SWC
# 3 - Reco VS GPP for Pulse and non-Pulse time
# You need to analyze these two df - summary2018_Pulse0 and summary2018_Pulse1 

# Non-pulse time
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanAT2)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanAT6)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanRH2)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanRH6)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanST5)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanST15)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanST30)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanSWC5)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanSWC15)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanSWC30)
cor.test(summary2019_Pulse0$meanRECO, summary2019_Pulse0$meanGPP)

# Pulse time
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanAT2)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanAT6)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanRH2)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanRH6)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanST5)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanST15)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanST30)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanSWC5)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanSWC15)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanSWC30)
cor.test(summary2019_Pulse1$meanRECO, summary2019_Pulse1$meanGPP)






################ Linear model ##############
model1 <- lm(RECO ~ SWC30 + ST30 + GPP, data = datarain19)
summary(model1)












