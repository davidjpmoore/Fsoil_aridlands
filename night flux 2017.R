# 29-11-2022
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

# upload 2017 fluxes and meteo
datarain17 =read.csv("datarain17_new.csv", header=TRUE, na.strings = "NaN")

Night17 <- datarain17 %>%
  filter(PPFD_IN == 0)

################# Make the same!!!!!!!!!!

Night17$DOY_S <- as.numeric(as.character(Night17$DOY_S))
Night17$RECO <- as.numeric(as.character(Night17$RECO), na.rm=TRUE)
Night17$GPP <- as.numeric(as.character(Night17$GPP), na.rm=TRUE)
Night17$NEE <- as.numeric(as.character(Night17$NEE), na.rm=TRUE)



summary2017_all_night <- Night17 %>%
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


summary2017_all_night$Season = vector(mode = 'character', length = nrow(summary2017_all_night))
summary2017_all_night$Season[summary2017_all_night$DOY_S %in% c(1:59,305:366)] = 'Winter'
summary2017_all_night$Season[summary2017_all_night$DOY_S %in% 60:181] = 'Spring'
summary2017_all_night$Season[summary2017_all_night$DOY_S %in% 182:304] = 'Summer'






summary2017_all_night$high_day <- paste(summary2017_all_night$sum_R>5)
summary2017_all_night$high_day <- as.numeric(summary2017_all_night$sum_R>5)

summary2017_all_night$Rain_DOY <- summary2017_all_night$high_day*summary2017_all_night$DOY_S

summary2017_all_night$Pulse_Days <- summary2017_all_night$high_day

### write summary file 
write.csv(summary2017_all_night, file = "summary2017_all_night.csv")


Pulses_2017_night <- summary2017_all_night %>%
  filter(sum_R > 5)

colnames(summary2017_all_night)[1]<-'DOY'
test <- data.frame(DOY=Pulses_2017_night$DOY_S)
test$day1<-test$DOY+1
test$day2<-test$DOY+2
test$day3<-test$DOY+3
test$day4<-test$DOY+4
test2<-melt(test)


test2<-data.frame(DOY=unique(test2$value))
test2$Pulse_DOY <- test2$DOY


summary2017_new_night <- merge(summary2017_all_night,test2,by="DOY",all.x=TRUE)

### write summary file 
write.csv(summary2017_new_night,  file = "summary2017_new_night.csv")


Fref = 0.75
SWCopt = 0.25

model99 <- nls(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = summary2017_new_night,
              start = list(SWCopt = 0.25, Fref=0.75)
)


summary(model99)
meanSWC30 =  years_sum1$meanSWC30
meanST30 =  years_sum1$meanST30
meanGPP =  years_sum1$meanGPP
Fref = -1.119e-15
SWCopt = 2.385e+01

RECOmod4 <- Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP
plot(RECOmod4)
plot(years_sum1$meanRECO)


cor(RECOmod4,years_sum1$meanRECO)

plot(years_sum1$meanRECO [RECOmod4>1], RECOmod4 [RECOmod4>1])

