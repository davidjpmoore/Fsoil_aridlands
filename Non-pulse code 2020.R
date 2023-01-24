# 08-08-2022
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
#library(Hmisc)
library(ggpubr)
library(ggplot2)
library(colorRamps)
library(reshape2)
library(zoo)

# upload 2020 fluxes and meteo
datarain20 =read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv",
                     header=TRUE, na.strings = "NaN")


# Make all steps to clean the initial document
datarain20$year=substr(datarain20$TIMESTAMP_START,1,4)
datarain20$month=substr(datarain20$TIMESTAMP_START, 5,6)
datarain20$day=substr(datarain20$TIMESTAMP_START, 7,8)
datarain20$hour=substr(datarain20$TIMESTAMP_START,9,10)
datarain20$min=substr(datarain20$TIMESTAMP_START,11,12)

datarain20$year1=substr(datarain20$TIMESTAMP_END,1,4)
datarain20$month1=substr(datarain20$TIMESTAMP_END, 5,6)
datarain20$day1=substr(datarain20$TIMESTAMP_END, 7,8)
datarain20$hour1=substr(datarain20$TIMESTAMP_END,9,10)
datarain20$min1=substr(datarain20$TIMESTAMP_END,11,12)

datarain20 <-  rename(datarain20, r=P, 
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

datarain20$dateStart <- paste(datarain20$year, datarain20$month, datarain20$day, sep="-")
datarain20$timeStart <- paste(datarain20$hour,datarain20$min, sep=":")
datarain20$dateEnd <- paste(datarain20$year1, datarain20$month1, datarain20$day1, sep="-")
datarain20$timeEnd <- paste(datarain20$hour1,datarain20$min1, sep=":")

datarain20$data_time_Start <- paste(datarain20$dateStart,datarain20$timeStart)
datarain20$data_time_End <- paste(datarain20$dateEnd,datarain20$timeEnd)

datarain20$data_time_Start = as.POSIXlt(datarain20$data_time_Start, format = "%Y-%m-%d %H:%M")
datarain20$data_time_End = as.POSIXlt(datarain20$data_time_End, format = "%Y-%m-%d %H:%M")

datarain20$high_precip <- datarain20$r>5
datarain20$high_precip <- as.numeric(datarain20$r>5)
datarain20$high_precip <- as.numeric(datarain20$r>5)*datarain20$r

datarain20$DOY_S <- paste(yday(datarain20$dateStart))
datarain20$DOY_E <- paste(yday(datarain20$dateEnd))

datarain20$RainEvent <- paste(datarain20$r>0)
datarain20$RainEvent <- as.numeric(datarain20$r>0)
datarain20$Rain_DOY <- as.numeric(datarain20$RainEvent)*as.numeric(datarain20$DOY_S)
sum(datarain20$RainEvent, na.rm=FALSE)

datarain20$highR_event <- datarain20$high_precip>5 
datarain20$highR_event <- as.numeric(as.logical(datarain20$highR_event))
sum(datarain20$highR_event) 

datarain20$Rain_DOY_high <- as.numeric(datarain20$highR_event)*as.numeric(datarain20$DOY_S)

Pulses_2020 <- datarain20 %>%
  filter(highR_event == 1)

# Calculate Pulse and Non-Pulse times fluxes
datarain20$DOY_S <- as.numeric(as.character(datarain20$DOY_S))

summary2020_all <- datarain20 %>%
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


summary2020_all$Season = vector(mode = 'character', length = nrow(summary2020_all))
summary2020_all$Season[summary2020_all$DOY_S %in% c(1:59,305:366)] = 'Winter'
summary2020_all$Season[summary2020_all$DOY_S %in% 60:181] = 'Spring'
summary2020_all$Season[summary2020_all$DOY_S %in% 182:304] = 'Summer'

summary2020_all$high_day <- paste(summary2020_all$sum_R>5)
summary2020_all$high_day <- as.numeric(summary2020_all$sum_R>5)

summary2020_all$Rain_DOY <- summary2020_all$high_day*summary2020_all$DOY_S

summary2020_all$Pulse_Days <- summary2020_all$high_day

### write summary file 
write.csv(summary2020_all, file = "summary2020_all.csv")

Pulses_2020_n <- summary2020_all %>%
  filter(sum_R > 5)

colnames(summary2020_all)[1]<-'DOY'
test <- data.frame(DOY=Pulses_2020_n$DOY_S)
test$day1<-test$DOY+1
test$day2<-test$DOY+2
test$day3<-test$DOY+3
test$day4<-test$DOY+4
test2<-melt(test)


test2<-data.frame(DOY=unique(test2$value))
test2$Pulse_DOY <- test2$DOY


summary2020_new <- merge(summary2020_all,test2,by="DOY",all.x=TRUE)

### write summary file 
write.csv(summary2020_new,  file = "summary2020_new.csv")


######## Make table just with NA-Pulse_DOY

summary2020_new[is.na(summary2020_new)] = 0

summary2020_Pulse0 <- summary2020_new %>%
  filter(Pulse_DOY == 0) 

summary2020_Pulse1 <- summary2020_new %>%
  filter(Pulse_DOY >= 63) 



plot(summary2020_all$DOY)
plot(summary2020_all$meanSWC30)


summary2020_new %>%
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
  ggtitle("Reco 2020")


summary2020_new %>%
  #filter(Pulse_Days ==1 ) %>%
  filter(Pulse_DOY == 0 ) %>%
  ggplot(aes(x= DOY, y = meanSWC5))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  #stat_smooth(method = "lm", 
  #formula = y ~ poly(x, 2),size = 1)
  #geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)+
  ggtitle("Reco 2020")



summary2020_new %>%
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
  ggtitle("Reco 2020")

summary2020_new %>%
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
  ggtitle("Reco 2020")

summary2020_Pulse0 %>%
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



summary2020_Pulse0 %>%
  ggplot(aes(x=DOY, y = meanSWC30))+
  geom_point(size=2, shape = 1)+
  geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))

#stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
#stat_smooth(method = "lm", 
#formula = y ~ poly(x, 2),size = 1)
#geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)


summary2020_Pulse0 %>%
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
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanAT2)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanAT6)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanRH2)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanRH6)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanST5)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanST15)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanST30)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanSWC5)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanSWC15)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanSWC30)
cor.test(summary2020_Pulse0$meanRECO, summary2020_Pulse0$meanGPP)

# Pulse time
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanAT2)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanAT6)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanRH2)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanRH6)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanST5)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanST15)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanST30)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanSWC5)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanSWC15)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanSWC30)
cor.test(summary2020_Pulse1$meanRECO, summary2020_Pulse1$meanGPP)

write.csv(summary2020_Pulse0, file = "data/NONpulse2020.csv")  

