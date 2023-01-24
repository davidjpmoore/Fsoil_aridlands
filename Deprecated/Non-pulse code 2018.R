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
#library(Hmisc)
library(ggpubr)
library(ggplot2)
library(colorRamps)
library(reshape2)
library(zoo)

# install.packages("Hmisc")

# upload 2018 fluxes and meteo
datarain18 =read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/AddedPartionedCflux_US-Wkg_HH_201712312330_201812312330.csv", 
                     header=TRUE, na.strings = "NaN")


datarainXX =read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/AddedPartionedCflux_US-Wkg_HH_201612312330_201712312330.csv", 
                     header=TRUE, na.strings = "NaN")


########### Here I tried to use the sate code as for other years but the date format here is different 
########### and the next steps are not possible yet.



# Make all steps to clean the initial document

datarain18$year=as.numeric(substr(datarainXX$TIMESTAMP_START,1,4))+1
datarain18$month=substr(datarainXX$TIMESTAMP_START, 5,6)
datarain18$day=substr(datarainXX$TIMESTAMP_START, 7,8)
datarain18$hour=substr(datarainXX$TIMESTAMP_START,9,10)
datarain18$min=substr(datarainXX$TIMESTAMP_START,11,12)

datarain18$year1=as.numeric(substr(datarainXX$TIMESTAMP_END,1,4))+1
datarain18$month1=substr(datarainXX$TIMESTAMP_END, 5,6)
datarain18$day1=substr(datarainXX$TIMESTAMP_END, 7,8)
datarain18$hour1=substr(datarainXX$TIMESTAMP_END,9,10)
datarain18$min1=substr(datarainXX$TIMESTAMP_END,11,12)

datarain18 <-  rename(datarain18, r=P, 
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

datarain18$dateStart <- paste(datarain18$year, datarain18$month, datarain18$day, sep="-")
datarain18$timeStart <- paste(datarain18$hour,datarain18$min, sep=":")
datarain18$dateEnd <- paste(datarain18$year1, datarain18$month1, datarain18$day1, sep="-")
datarain18$timeEnd <- paste(datarain18$hour1,datarain18$min1, sep=":")

datarain18$data_time_Start <- paste(datarain18$dateStart,datarain18$timeStart)
datarain18$data_time_End <- paste(datarain18$dateEnd,datarain18$timeEnd)

datarain18$data_time_Start = as.POSIXlt(datarain18$data_time_Start, format = "%Y-%m-%d %H:%M")
datarain18$data_time_End = as.POSIXlt(datarain18$data_time_End, format = "%Y-%m-%d %H:%M")

datarain18$high_precip <- datarain18$r>5
datarain18$high_precip <- as.numeric(datarain18$r>5)
datarain18$high_precip <- as.numeric(datarain18$r>5)*datarain18$r

datarain18$DOY_S <- paste(yday(datarain18$dateStart))
datarain18$DOY_E <- paste(yday(datarain18$dateEnd))

datarain18$RainEvent <- paste(datarain18$r>0)
datarain18$RainEvent <- as.numeric(datarain18$r>0)
datarain18$Rain_DOY <- as.numeric(datarain18$RainEvent)*as.numeric(datarain18$DOY_S)
sum(datarain18$RainEvent, na.rm=FALSE)

datarain18$highR_event <- datarain18$high_precip>5 
datarain18$highR_event <- as.numeric(as.logical(datarain18$highR_event))
sum(datarain18$highR_event) 

datarain18$Rain_DOY_high <- as.numeric(datarain18$highR_event)*as.numeric(datarain18$DOY_S)

Pulses_2018 <- datarain18 %>%
  filter(highR_event == 1)

# Calculate Pulse and Non-Pulse times fluxes
datarain18$DOY_S <- as.numeric(as.character(datarain18$DOY_S))

summary2018_all <- datarain18 %>%
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


summary2018_all$Season = vector(mode = 'character', length = nrow(summary2018_all))
summary2018_all$Season[summary2018_all$DOY_S %in% c(1:59,305:366)] = 'Winter'
summary2018_all$Season[summary2018_all$DOY_S %in% 60:181] = 'Spring'
summary2018_all$Season[summary2018_all$DOY_S %in% 182:304] = 'Summer'

summary2018_all$high_day <- paste(summary2018_all$sum_R>5)
summary2018_all$high_day <- as.numeric(summary2018_all$sum_R>5)

summary2018_all$Rain_DOY <- summary2018_all$high_day*summary2018_all$DOY_S

summary2018_all$Pulse_Days <- summary2018_all$high_day

Pulses_2018_n <- summary2018_all %>%
  filter(sum_R > 5)

colnames(summary2018_all)[1]<-'DOY'
test <- data.frame(DOY=Pulses_2018_n$DOY_S)
test$day1<-test$DOY+1
test$day2<-test$DOY+2
test$day3<-test$DOY+3
test$day4<-test$DOY+4
test2<-melt(test)


test2<-data.frame(DOY=unique(test2$value))
test2$Pulse_DOY <- test2$DOY


summary2018_new <- merge(summary2018_all,test2,by="DOY",all.x=TRUE)

######## Make table just with NA-Pulse_DOY

summary2018_new[is.na(summary2018_new)] = 0

summary2018_Pulse0 <- summary2018_new %>%
  filter(Pulse_DOY == 0) 

summary2018_Pulse1 <- summary2018_new %>%
  filter(Pulse_DOY >= 15) 



plot(summary2018_all$DOY)
plot(summary2018_all$meanSWC30)

summary2018_new %>%
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
  ggtitle("Reco 2018")


summary2018_new %>%
  #filter(Pulse_Days ==1 ) %>%
  filter(Pulse_DOY == 0 ) %>%
  ggplot(aes(x=DOY, y = meanSWC5))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  #stat_smooth(method = "lm", 
  #formula = y ~ poly(x, 2),size = 1)
  #geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)+
  ggtitle("Reco 2018")




summary2018_new %>%
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
  ggtitle("Reco 2018")

summary2018_new %>%
  #filter(Pulse_Days ==1 ) %>%
  filter(Pulse_DOY >= 46 ) %>%
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
  ggtitle("Reco 2018")

summary2018_Pulse0 %>%
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



summary2018_Pulse0 %>%
  ggplot(aes(x=DOY, y = meanSWC5))+
  geom_point(size=2, shape = 1)+
  geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))

#stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
#stat_smooth(method = "lm", 
#formula = y ~ poly(x, 2),size = 1)
#geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)


summary2018_Pulse0 %>%
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
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanAT2)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanAT6)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanRH2)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanRH6)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanST5)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanST15)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanST30)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanSWC5)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanSWC15)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanSWC30)
cor.test(summary2018_Pulse0$meanRECO, summary2018_Pulse0$meanGPP)

# Pulse time
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanAT2)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanAT6)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanRH2)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanRH6)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanST5)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanST15)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanST30)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanSWC5)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanSWC15)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanSWC30)
cor.test(summary2018_Pulse1$meanRECO, summary2018_Pulse1$meanGPP)

# 
# git config --global user.email "sunlife1408@yandex.ru"
# git config --global user.name "Anastasia Makhnykina"
# 
# 

write_csv(summary2018_Pulse0, "data/NONpulse2018.csv")

summary2018_Pulse0


write.csv(datarain18, file="datarain18.csv")

################ Linear model ##############
model1 <- lm(RECO ~ SWC30 + ST30 + GPP, data = datarain18)
summary(model1)













