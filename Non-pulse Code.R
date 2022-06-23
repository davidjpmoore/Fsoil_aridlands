#6/22/2022
#Anastasia Makhnykina

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


#open cvs
datarain=read.csv("data/datarain.csv", 
                  header=TRUE, na.strings = "NaN")


datarain$DOY_S <- as.numeric(as.integer(datarain$DOY_S))
 
summary2 <- datarain %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr :: summarise(meanAT2=mean(replace(AT2, AT2== -9999, NA),na.rm=TRUE), 
                     meanAT6=mean(replace(AT6, AT6== -9999, NA),na.rm=TRUE),
                     sum_R=mean(sum_rain, na.rm=TRUE),
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
                     meanRECO=mean(RECO, na.rm=TRUE), sdReco=sd(RECO, na.rm=TRUE))


summary2$Season = vector(mode = 'character', length = nrow(summary2))
summary2$Season[summary2$`as.numeric(DOY_S)` %in% c(1:59,305:366)] = 'Winter'
summary2$Season[summary2$`as.numeric(DOY_S)` %in% 60:181] = 'Spring'
summary2$Season[summary2$`as.numeric(DOY_S)` %in% 182:304] = 'Summer'


#Exclude the rows with Pulses and 5 days after 
#Make in summary - marking of Day0 - Day5

summary2$high_day <- paste(summary2$sum_R>5)
summary2$high_day <- as.numeric(summary2$sum_R>5)

summary2$Rain_DOY <- summary2$high_day*as.numeric(summary2$`as.numeric(DOY_S)`)

summary2$Pulse_Days <- summary2$high_day 

Pulses <- summary2 %>%
  filter(sum_R > 5)
############# LONGER WAY

summary2$Pulse1 = vector(mode = 'numeric', length = nrow(summary2))
summary2$Pulse1 [summary2$`as.numeric(DOY_S)` %in% 14] = '1'
summary2$Pulse1 [summary2$`as.numeric(DOY_S)` %in% 15] = '2'
summary2$Pulse1 [summary2$`as.numeric(DOY_S)` %in% 16] = '3'
summary2$Pulse1 [summary2$`as.numeric(DOY_S)` %in% 17] = '4'
summary2$Pulse1 [summary2$`as.numeric(DOY_S)` %in% 18] = '5'


###################### FASTER WAY

colnames(summary2)[1]<-'DOY'
test <- data.frame(DOY=Pulses$`as.numeric(DOY_S)`)
test$day1<-test$DOY+1
test$day2<-test$DOY+2
test$day3<-test$DOY+3
test$day4<-test$DOY+4
test2<-melt(test)


test2<-data.frame(DOY=unique(test2$value))
test2$Pulse_DOY <- test2$DOY


summary4 <- merge(summary2,test2,by="DOY",all.x=TRUE)

######## Make table just with NA-Pulse_DOY

summary4[is.na(summary4)] = 0

summary5 <- summary4 %>%
  filter(Pulse_DOY == 0) 


summary4 %>%
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
  geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)


Deviation <- mean(summary4$meanRECO) - mean(summary5$meanRECO)

Per_Pulse <- 100 - (mean(summary5$meanRECO)*100)/mean(summary4$meanRECO)

mean(summary4$meanRECO[summary4$Pulse_DOY > 0])

Per_Pulse1 <- 100 - (mean(summary5$meanRECO)*100)/mean(summary4$meanRECO[summary4$Pulse_DOY > 0])



summary5 %>%
  ggplot(aes(x=DOY, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  #stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2),size = 1)
  geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)



summary5 %>%
  ggplot(aes(x=DOY, y = meanSWC30))+
  geom_point(size=2, shape = 1)+
  geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))

  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  #stat_smooth(method = "lm", 
  #formula = y ~ poly(x, 2),size = 1)
  #geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)


summary5 %>%
  ggplot(aes(x=meanSWC30, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2),size = 1
              )



############ moving average ++++ other smooothing functions - Smoothing 3 non-pulse Reco
plot(summary5$DOY, summary5$meanRECO)
lines(lowess(summary5$DOY, summary5$meanRECO), col='red')

summary5 %>%
  ggplot(aes(x=DOY, y = meanRECO))+
  geom_point(size=2, shape = 1)+
  geom_smooth(span=0.1)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  #stat_smooth(method = "lm", 
  #formula = y ~ poly(x, 2),size = 1)
  #geom_errorbar(aes(ymin = meanRECO- sdReco, ymax= meanRECO+sdReco), width = 3)



summary5$date<- as.Date(summary5$DOY,  origin = "2017-01-01")


plot(df.ts)
lines(ts.2day.mean, col = 'red')

require(dplyr)

summary6 <- summary5 %>% 
  group_by(DOY) %>% 
  mutate(rec = 1) %>% 
  mutate(rollavg = cumsum(meanRECO)/cumsum(rec)) %>% 
  select(-rec)


plot(summary6$DOY, summary6$rollavg)


















