#05/02/2022
#Anastasia Makhnykina
#grab data we need

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


#Load Datarain
setwd("C:\\Users\\sunlife1408\\Documents\\RainMan2022\\Fsoil_aridlands\\data")

datarain=read.csv("C:/Users/sunlife1408/Documents/RainMan2022/Fsoil_aridlands/data/datarain_processed.csv", header=TRUE, na.strings = "NaN")

#remove unnessessary columns

datarain$NEE<- as.numeric(as.character(datarain$NEE))
datarain$RECO <- as.numeric(as.character(datarain$RECO))
datarain$GPP <- as.numeric(as.character(datarain$GPP))


#Create Pulses 2-18
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
  filter(DOY_S %in% (14:35)) 


Pulse4 <- 
  datarain %>%
  select (PA, AT2, RH2, AT6, RH6, r, SWC5, 
          SWC15, SWC30, SWC30, ST5, 
          ST15,ST30, NEE, RECO, GPP, high_precip, 
          RainEvent, dateStart, dateEnd, data_time_Start,
          data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain) %>%
  filter(DOY_S %in% (165:186)) 

Pulse5 <- 
  datarain %>%
  filter(DOY_S %in% (169:190)) 
Pulse5_pro <- select(Pulse5, c(PA, AT2, RH2, AT6, RH6, r, SWC5, 
                               SWC15, SWC30, SWC30, ST5, 
                               ST15,ST30, NEE, RECO, GPP, high_precip, 
                               RainEvent, dateStart, dateEnd, data_time_Start,
                               data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain))

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


###########Plotting some information inside the Pulse

###### The best 3rd Pulse!

Pulse3_sum$meanday <- as.numeric(as.double(Pulse3_sum$meanday))

Pulse3_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=meanday, y=meanRECO))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('Day after Rain')+
  ylab('Mean Reco (micromol m-2 s-1)')+
  theme(text = element_text(size = 20))

Pulse3_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse3')

Pulse2_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse2')

Pulse1_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean Reco (micromol m-2 s-1)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse1')


Pulse4_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse4')

Pulse5_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse5')

Pulse6_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse6')

Pulse7_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse7')

Pulse8_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse8')

Pulse9_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse9')

Pulse10_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse10')

Pulse11_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse11')

Pulse12_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse12')

Pulse13_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse13')

Pulse14_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse14')

Pulse15_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse15')

Pulse16_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse16')

Pulse17_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse17')

Pulse18_sum %>%
  filter(meanday %in% 0:14)%>%
  ggplot(aes(x=`as.numeric(DOY_S)`, y=meanSWC5))+
  geom_point(size=2)+
  geom_line()+
  theme_classic()+
  xlab('DOY')+
  ylab('Mean SWC, 5 cm (m3 m-3)')+
  theme(text = element_text(size = 20))+
  ggtitle('Pulse18')



# Make a summary table for the all 18 pulses = 16 variables + 22*18 rows should be Pulse(n)_sum files!!! = 306 columns

Pulse1$Day [Pulse1$DOY_S %in% 7] = '-7'
Pulse1$Day [Pulse1$DOY_S %in% 8] = '-6'
Pulse1$Day [Pulse1$DOY_S %in% 9] = '-5'
Pulse1$Day [Pulse1$DOY_S %in% 10] = '-4'
Pulse1$Day [Pulse1$DOY_S %in% 11] = '-3'
Pulse1$Day [Pulse1$DOY_S %in% 12] = '-2'
Pulse1$Day [Pulse1$DOY_S %in% 13] = '-1'
Pulse1$Day [Pulse1$DOY_S %in% 14] = '0'
Pulse1$Day [Pulse1$DOY_S %in% 15] = '1'
Pulse1$Day [Pulse1$DOY_S %in% 16] = '2'
Pulse1$Day [Pulse1$DOY_S %in% 17] = '3'
Pulse1$Day [Pulse1$DOY_S %in% 18] = '4'
Pulse1$Day [Pulse1$DOY_S %in% 19] = '5'
Pulse1$Day [Pulse1$DOY_S %in% 20] = '6'
Pulse1$Day [Pulse1$DOY_S %in% 21] = '7'
Pulse1$Day [Pulse1$DOY_S %in% 22] = '8'
Pulse1$Day [Pulse1$DOY_S %in% 23] = '9'
Pulse1$Day [Pulse1$DOY_S %in% 24] = '10'
Pulse1$Day [Pulse1$DOY_S %in% 25] = '11'
Pulse1$Day [Pulse1$DOY_S %in% 26] = '12'
Pulse1$Day [Pulse1$DOY_S %in% 27] = '13'
Pulse1$Day [Pulse1$DOY_S %in% 28] = '14'


Pulse2$Day [Pulse2$DOY_S %in% 8] = '-7'
Pulse2$Day [Pulse2$DOY_S %in% 9] = '-6'
Pulse2$Day [Pulse2$DOY_S %in% 10] = '-5'
Pulse2$Day [Pulse2$DOY_S %in% 11] = '-4'
Pulse2$Day [Pulse2$DOY_S %in% 12] = '-3'
Pulse2$Day [Pulse2$DOY_S %in% 13] = '-2'
Pulse2$Day [Pulse2$DOY_S %in% 14] = '-1'
Pulse2$Day [Pulse2$DOY_S %in% 15] = '0'
Pulse2$Day [Pulse2$DOY_S %in% 16] = '1'
Pulse2$Day [Pulse2$DOY_S %in% 17] = '2'
Pulse2$Day [Pulse2$DOY_S %in% 18] = '3'
Pulse2$Day [Pulse2$DOY_S %in% 19] = '4'
Pulse2$Day [Pulse2$DOY_S %in% 20] = '5'
Pulse2$Day [Pulse2$DOY_S %in% 21] = '6'
Pulse2$Day [Pulse2$DOY_S %in% 22] = '7'
Pulse2$Day [Pulse2$DOY_S %in% 23] = '8'
Pulse2$Day [Pulse2$DOY_S %in% 24] = '9'
Pulse2$Day [Pulse2$DOY_S %in% 25] = '10'
Pulse2$Day [Pulse2$DOY_S %in% 26] = '11'
Pulse2$Day [Pulse2$DOY_S %in% 27] = '12'
Pulse2$Day [Pulse2$DOY_S %in% 28] = '13'
Pulse2$Day [Pulse2$DOY_S %in% 29] = '14'


Pulse3$Day [Pulse3$DOY_S %in% 14] = '-7'
Pulse3$Day [Pulse3$DOY_S %in% 15] = '-6'
Pulse3$Day [Pulse3$DOY_S %in% 16] = '-5'
Pulse3$Day [Pulse3$DOY_S %in% 17] = '-4'
Pulse3$Day [Pulse3$DOY_S %in% 18] = '-3'
Pulse3$Day [Pulse3$DOY_S %in% 19] = '-2'
Pulse3$Day [Pulse3$DOY_S %in% 20] = '-1'
Pulse3$Day [Pulse3$DOY_S %in% 21] = '0'
Pulse3$Day [Pulse3$DOY_S %in% 22] = '1'
Pulse3$Day [Pulse3$DOY_S %in% 23] = '2'
Pulse3$Day [Pulse3$DOY_S %in% 24] = '3'
Pulse3$Day [Pulse3$DOY_S %in% 25] = '4'
Pulse3$Day [Pulse3$DOY_S %in% 26] = '5'
Pulse3$Day [Pulse3$DOY_S %in% 27] = '6'
Pulse3$Day [Pulse3$DOY_S %in% 28] = '7'
Pulse3$Day [Pulse3$DOY_S %in% 29] = '8'
Pulse3$Day [Pulse3$DOY_S %in% 30] = '9'
Pulse3$Day [Pulse3$DOY_S %in% 31] = '10'
Pulse3$Day [Pulse3$DOY_S %in% 32] = '11'
Pulse3$Day [Pulse3$DOY_S %in% 33] = '12'
Pulse3$Day [Pulse3$DOY_S %in% 34] = '13'
Pulse3$Day [Pulse3$DOY_S %in% 35] = '14'


Pulse4$Day [Pulse4$DOY_S %in% 165] = '-7'
Pulse4$Day [Pulse4$DOY_S %in% 166] = '-6'
Pulse4$Day [Pulse4$DOY_S %in% 167] = '-5'
Pulse4$Day [Pulse4$DOY_S %in% 168] = '-4'
Pulse4$Day [Pulse4$DOY_S %in% 169] = '-3'
Pulse4$Day [Pulse4$DOY_S %in% 170] = '-2'
Pulse4$Day [Pulse4$DOY_S %in% 171] = '-1'
Pulse4$Day [Pulse4$DOY_S %in% 172] = '0'
Pulse4$Day [Pulse4$DOY_S %in% 173] = '1'
Pulse4$Day [Pulse4$DOY_S %in% 174] = '2'
Pulse4$Day [Pulse4$DOY_S %in% 175] = '3'
Pulse4$Day [Pulse4$DOY_S %in% 176] = '4'
Pulse4$Day [Pulse4$DOY_S %in% 177] = '5'
Pulse4$Day [Pulse4$DOY_S %in% 178] = '6'
Pulse4$Day [Pulse4$DOY_S %in% 179] = '7'
Pulse4$Day [Pulse4$DOY_S %in% 180] = '8'
Pulse4$Day [Pulse4$DOY_S %in% 181] = '9'
Pulse4$Day [Pulse4$DOY_S %in% 182] = '10'
Pulse4$Day [Pulse4$DOY_S %in% 183] = '11'
Pulse4$Day [Pulse4$DOY_S %in% 184] = '12'
Pulse4$Day [Pulse4$DOY_S %in% 185] = '13'
Pulse4$Day [Pulse4$DOY_S %in% 186] = '14'


Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 169] = '-7'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 170] = '-6'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 171] = '-5'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 172] = '-4'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 173] = '-3'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 174] = '-2'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 175] = '-1'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 176] = '0'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 177] = '1'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 178] = '2'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 179] = '3'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 180] = '4'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 181] = '5'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 182] = '6'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 183] = '7'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 184] = '8'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 185] = '9'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 186] = '10'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 187] = '11'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 188] = '12'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 189] = '13'
Pulse5_pro$Day [Pulse5_pro$DOY_S %in% 190] = '14'


Pulse6$Day [Pulse6$DOY_S %in% 177] = '-7'
Pulse6$Day [Pulse6$DOY_S %in% 178] = '-6'
Pulse6$Day [Pulse6$DOY_S %in% 179] = '-5'
Pulse6$Day [Pulse6$DOY_S %in% 180] = '-4'
Pulse6$Day [Pulse6$DOY_S %in% 181] = '-3'
Pulse6$Day [Pulse6$DOY_S %in% 182] = '-2'
Pulse6$Day [Pulse6$DOY_S %in% 183] = '-1'
Pulse6$Day [Pulse6$DOY_S %in% 184] = '0'
Pulse6$Day [Pulse6$DOY_S %in% 185] = '1'
Pulse6$Day [Pulse6$DOY_S %in% 186] = '2'
Pulse6$Day [Pulse6$DOY_S %in% 187] = '3'
Pulse6$Day [Pulse6$DOY_S %in% 188] = '4'
Pulse6$Day [Pulse6$DOY_S %in% 189] = '5'
Pulse6$Day [Pulse6$DOY_S %in% 190] = '6'
Pulse6$Day [Pulse6$DOY_S %in% 191] = '7'
Pulse6$Day [Pulse6$DOY_S %in% 192] = '8'
Pulse6$Day [Pulse6$DOY_S %in% 193] = '9'
Pulse6$Day [Pulse6$DOY_S %in% 194] = '10'
Pulse6$Day [Pulse6$DOY_S %in% 195] = '11'
Pulse6$Day [Pulse6$DOY_S %in% 196] = '12'
Pulse6$Day [Pulse6$DOY_S %in% 197] = '13'
Pulse6$Day [Pulse6$DOY_S %in% 198] = '14'


Pulse7$Day [Pulse7$DOY_S %in% 183] = '-7'
Pulse7$Day [Pulse7$DOY_S %in% 184] = '-6'
Pulse7$Day [Pulse7$DOY_S %in% 185] = '-5'
Pulse7$Day [Pulse7$DOY_S %in% 186] = '-4'
Pulse7$Day [Pulse7$DOY_S %in% 187] = '-3'
Pulse7$Day [Pulse7$DOY_S %in% 188] = '-2'
Pulse7$Day [Pulse7$DOY_S %in% 189] = '-1'
Pulse7$Day [Pulse7$DOY_S %in% 190] = '0'
Pulse7$Day [Pulse7$DOY_S %in% 191] = '1'
Pulse7$Day [Pulse7$DOY_S %in% 192] = '2'
Pulse7$Day [Pulse7$DOY_S %in% 193] = '3'
Pulse7$Day [Pulse7$DOY_S %in% 194] = '4'
Pulse7$Day [Pulse7$DOY_S %in% 195] = '5'
Pulse7$Day [Pulse7$DOY_S %in% 196] = '6'
Pulse7$Day [Pulse7$DOY_S %in% 197] = '7'
Pulse7$Day [Pulse7$DOY_S %in% 198] = '8'
Pulse7$Day [Pulse7$DOY_S %in% 199] = '9'
Pulse7$Day [Pulse7$DOY_S %in% 200] = '10'
Pulse7$Day [Pulse7$DOY_S %in% 201] = '11'
Pulse7$Day [Pulse7$DOY_S %in% 202] = '12'
Pulse7$Day [Pulse7$DOY_S %in% 203] = '13'
Pulse7$Day [Pulse7$DOY_S %in% 204] = '14'


Pulse8$Day [Pulse8$DOY_S %in% 186] = '-7'
Pulse8$Day [Pulse8$DOY_S %in% 187] = '-6'
Pulse8$Day [Pulse8$DOY_S %in% 188] = '-5'
Pulse8$Day [Pulse8$DOY_S %in% 189] = '-4'
Pulse8$Day [Pulse8$DOY_S %in% 190] = '-3'
Pulse8$Day [Pulse8$DOY_S %in% 191] = '-2'
Pulse8$Day [Pulse8$DOY_S %in% 192] = '-1'
Pulse8$Day [Pulse8$DOY_S %in% 193] = '0'
Pulse8$Day [Pulse8$DOY_S %in% 194] = '1'
Pulse8$Day [Pulse8$DOY_S %in% 195] = '2'
Pulse8$Day [Pulse8$DOY_S %in% 196] = '3'
Pulse8$Day [Pulse8$DOY_S %in% 197] = '4'
Pulse8$Day [Pulse8$DOY_S %in% 198] = '5'
Pulse8$Day [Pulse8$DOY_S %in% 199] = '6'
Pulse8$Day [Pulse8$DOY_S %in% 200] = '7'
Pulse8$Day [Pulse8$DOY_S %in% 201] = '8'
Pulse8$Day [Pulse8$DOY_S %in% 202] = '9'
Pulse8$Day [Pulse8$DOY_S %in% 203] = '10'
Pulse8$Day [Pulse8$DOY_S %in% 204] = '11'
Pulse8$Day [Pulse8$DOY_S %in% 205] = '12'
Pulse8$Day [Pulse8$DOY_S %in% 206] = '13'
Pulse8$Day [Pulse8$DOY_S %in% 207] = '14'


Pulse9$Day [Pulse9$DOY_S %in% 187] = '-7'
Pulse9$Day [Pulse9$DOY_S %in% 188] = '-6'
Pulse9$Day [Pulse9$DOY_S %in% 189] = '-5'
Pulse9$Day [Pulse9$DOY_S %in% 190] = '-4'
Pulse9$Day [Pulse9$DOY_S %in% 191] = '-3'
Pulse9$Day [Pulse9$DOY_S %in% 192] = '-2'
Pulse9$Day [Pulse9$DOY_S %in% 193] = '-1'
Pulse9$Day [Pulse9$DOY_S %in% 194] = '0'
Pulse9$Day [Pulse9$DOY_S %in% 195] = '1'
Pulse9$Day [Pulse9$DOY_S %in% 196] = '2'
Pulse9$Day [Pulse9$DOY_S %in% 197] = '3'
Pulse9$Day [Pulse9$DOY_S %in% 198] = '4'
Pulse9$Day [Pulse9$DOY_S %in% 199] = '5'
Pulse9$Day [Pulse9$DOY_S %in% 200] = '6'
Pulse9$Day [Pulse9$DOY_S %in% 201] = '7'
Pulse9$Day [Pulse9$DOY_S %in% 202] = '8'
Pulse9$Day [Pulse9$DOY_S %in% 203] = '9'
Pulse9$Day [Pulse9$DOY_S %in% 204] = '10'
Pulse9$Day [Pulse9$DOY_S %in% 205] = '11'
Pulse9$Day [Pulse9$DOY_S %in% 206] = '12'
Pulse9$Day [Pulse9$DOY_S %in% 207] = '13'
Pulse9$Day [Pulse9$DOY_S %in% 208] = '14'


Pulse10$Day [Pulse10$DOY_S %in% 191] = '-7'
Pulse10$Day [Pulse10$DOY_S %in% 192] = '-6'
Pulse10$Day [Pulse10$DOY_S %in% 193] = '-5'
Pulse10$Day [Pulse10$DOY_S %in% 194] = '-4'
Pulse10$Day [Pulse10$DOY_S %in% 195] = '-3'
Pulse10$Day [Pulse10$DOY_S %in% 196] = '-2'
Pulse10$Day [Pulse10$DOY_S %in% 197] = '-1'
Pulse10$Day [Pulse10$DOY_S %in% 198] = '0'
Pulse10$Day [Pulse10$DOY_S %in% 199] = '1'
Pulse10$Day [Pulse10$DOY_S %in% 200] = '2'
Pulse10$Day [Pulse10$DOY_S %in% 201] = '3'
Pulse10$Day [Pulse10$DOY_S %in% 202] = '4'
Pulse10$Day [Pulse10$DOY_S %in% 203] = '5'
Pulse10$Day [Pulse10$DOY_S %in% 204] = '6'
Pulse10$Day [Pulse10$DOY_S %in% 205] = '7'
Pulse10$Day [Pulse10$DOY_S %in% 206] = '8'
Pulse10$Day [Pulse10$DOY_S %in% 207] = '9'
Pulse10$Day [Pulse10$DOY_S %in% 208] = '10'
Pulse10$Day [Pulse10$DOY_S %in% 209] = '11'
Pulse10$Day [Pulse10$DOY_S %in% 210] = '12'
Pulse10$Day [Pulse10$DOY_S %in% 211] = '13'
Pulse10$Day [Pulse10$DOY_S %in% 212] = '14'


Pulse11$Day [Pulse11$DOY_S %in% 200] = '-7'
Pulse11$Day [Pulse11$DOY_S %in% 201] = '-6'
Pulse11$Day [Pulse11$DOY_S %in% 202] = '-5'
Pulse11$Day [Pulse11$DOY_S %in% 203] = '-4'
Pulse11$Day [Pulse11$DOY_S %in% 204] = '-3'
Pulse11$Day [Pulse11$DOY_S %in% 205] = '-2'
Pulse11$Day [Pulse11$DOY_S %in% 206] = '-1'
Pulse11$Day [Pulse11$DOY_S %in% 207] = '0'
Pulse11$Day [Pulse11$DOY_S %in% 208] = '1'
Pulse11$Day [Pulse11$DOY_S %in% 209] = '2'
Pulse11$Day [Pulse11$DOY_S %in% 210] = '3'
Pulse11$Day [Pulse11$DOY_S %in% 211] = '4'
Pulse11$Day [Pulse11$DOY_S %in% 212] = '5'
Pulse11$Day [Pulse11$DOY_S %in% 213] = '6'
Pulse11$Day [Pulse11$DOY_S %in% 214] = '7'
Pulse11$Day [Pulse11$DOY_S %in% 215] = '8'
Pulse11$Day [Pulse11$DOY_S %in% 216] = '9'
Pulse11$Day [Pulse11$DOY_S %in% 217] = '10'
Pulse11$Day [Pulse11$DOY_S %in% 218] = '11'
Pulse11$Day [Pulse11$DOY_S %in% 219] = '12'
Pulse11$Day [Pulse11$DOY_S %in% 220] = '13'
Pulse11$Day [Pulse11$DOY_S %in% 221] = '14'


Pulse12$Day [Pulse12$DOY_S %in% 201] = '-7'
Pulse12$Day [Pulse12$DOY_S %in% 202] = '-6'
Pulse12$Day [Pulse12$DOY_S %in% 203] = '-5'
Pulse12$Day [Pulse12$DOY_S %in% 204] = '-4'
Pulse12$Day [Pulse12$DOY_S %in% 205] = '-3'
Pulse12$Day [Pulse12$DOY_S %in% 206] = '-2'
Pulse12$Day [Pulse12$DOY_S %in% 207] = '-1'
Pulse12$Day [Pulse12$DOY_S %in% 208] = '0'
Pulse12$Day [Pulse12$DOY_S %in% 209] = '1'
Pulse12$Day [Pulse12$DOY_S %in% 210] = '2'
Pulse12$Day [Pulse12$DOY_S %in% 211] = '3'
Pulse12$Day [Pulse12$DOY_S %in% 212] = '4'
Pulse12$Day [Pulse12$DOY_S %in% 213] = '5'
Pulse12$Day [Pulse12$DOY_S %in% 214] = '6'
Pulse12$Day [Pulse12$DOY_S %in% 215] = '7'
Pulse12$Day [Pulse12$DOY_S %in% 216] = '8'
Pulse12$Day [Pulse12$DOY_S %in% 217] = '9'
Pulse12$Day [Pulse12$DOY_S %in% 218] = '10'
Pulse12$Day [Pulse12$DOY_S %in% 219] = '11'
Pulse12$Day [Pulse12$DOY_S %in% 220] = '12'
Pulse12$Day [Pulse12$DOY_S %in% 221] = '13'
Pulse12$Day [Pulse12$DOY_S %in% 222] = '14'


Pulse13$Day [Pulse13$DOY_S %in% 202] = '-7'
Pulse13$Day [Pulse13$DOY_S %in% 203] = '-6'
Pulse13$Day [Pulse13$DOY_S %in% 204] = '-5'
Pulse13$Day [Pulse13$DOY_S %in% 205] = '-4'
Pulse13$Day [Pulse13$DOY_S %in% 206] = '-3'
Pulse13$Day [Pulse13$DOY_S %in% 207] = '-2'
Pulse13$Day [Pulse13$DOY_S %in% 208] = '-1'
Pulse13$Day [Pulse13$DOY_S %in% 209] = '0'
Pulse13$Day [Pulse13$DOY_S %in% 210] = '1'
Pulse13$Day [Pulse13$DOY_S %in% 211] = '2'
Pulse13$Day [Pulse13$DOY_S %in% 212] = '3'
Pulse13$Day [Pulse13$DOY_S %in% 213] = '4'
Pulse13$Day [Pulse13$DOY_S %in% 214] = '5'
Pulse13$Day [Pulse13$DOY_S %in% 215] = '6'
Pulse13$Day [Pulse13$DOY_S %in% 216] = '7'
Pulse13$Day [Pulse13$DOY_S %in% 217] = '8'
Pulse13$Day [Pulse13$DOY_S %in% 218] = '9'
Pulse13$Day [Pulse13$DOY_S %in% 219] = '10'
Pulse13$Day [Pulse13$DOY_S %in% 220] = '11'
Pulse13$Day [Pulse13$DOY_S %in% 221] = '12'
Pulse13$Day [Pulse13$DOY_S %in% 222] = '13'
Pulse13$Day [Pulse13$DOY_S %in% 223] = '14'


Pulse14$Day [Pulse14$DOY_S %in% 206] = '-7'
Pulse14$Day [Pulse14$DOY_S %in% 207] = '-6'
Pulse14$Day [Pulse14$DOY_S %in% 208] = '-5'
Pulse14$Day [Pulse14$DOY_S %in% 209] = '-4'
Pulse14$Day [Pulse14$DOY_S %in% 210] = '-3'
Pulse14$Day [Pulse14$DOY_S %in% 211] = '-2'
Pulse14$Day [Pulse14$DOY_S %in% 212] = '-1'
Pulse14$Day [Pulse14$DOY_S %in% 213] = '0'
Pulse14$Day [Pulse14$DOY_S %in% 214] = '1'
Pulse14$Day [Pulse14$DOY_S %in% 215] = '2'
Pulse14$Day [Pulse14$DOY_S %in% 216] = '3'
Pulse14$Day [Pulse14$DOY_S %in% 217] = '4'
Pulse14$Day [Pulse14$DOY_S %in% 218] = '5'
Pulse14$Day [Pulse14$DOY_S %in% 219] = '6'
Pulse14$Day [Pulse14$DOY_S %in% 220] = '7'
Pulse14$Day [Pulse14$DOY_S %in% 221] = '8'
Pulse14$Day [Pulse14$DOY_S %in% 222] = '9'
Pulse14$Day [Pulse14$DOY_S %in% 223] = '10'
Pulse14$Day [Pulse14$DOY_S %in% 224] = '11'
Pulse14$Day [Pulse14$DOY_S %in% 225] = '12'
Pulse14$Day [Pulse14$DOY_S %in% 226] = '13'
Pulse14$Day [Pulse14$DOY_S %in% 227] = '14'


Pulse15$Day [Pulse15$DOY_S %in% 207] = '-7'
Pulse15$Day [Pulse15$DOY_S %in% 208] = '-6'
Pulse15$Day [Pulse15$DOY_S %in% 209] = '-5'
Pulse15$Day [Pulse15$DOY_S %in% 210] = '-4'
Pulse15$Day [Pulse15$DOY_S %in% 211] = '-3'
Pulse15$Day [Pulse15$DOY_S %in% 212] = '-2'
Pulse15$Day [Pulse15$DOY_S %in% 213] = '-1'
Pulse15$Day [Pulse15$DOY_S %in% 214] = '0'
Pulse15$Day [Pulse15$DOY_S %in% 215] = '1'
Pulse15$Day [Pulse15$DOY_S %in% 216] = '2'
Pulse15$Day [Pulse15$DOY_S %in% 217] = '3'
Pulse15$Day [Pulse15$DOY_S %in% 218] = '4'
Pulse15$Day [Pulse15$DOY_S %in% 219] = '5'
Pulse15$Day [Pulse15$DOY_S %in% 220] = '6'
Pulse15$Day [Pulse15$DOY_S %in% 221] = '7'
Pulse15$Day [Pulse15$DOY_S %in% 222] = '8'
Pulse15$Day [Pulse15$DOY_S %in% 223] = '9'
Pulse15$Day [Pulse15$DOY_S %in% 224] = '10'
Pulse15$Day [Pulse15$DOY_S %in% 225] = '11'
Pulse15$Day [Pulse15$DOY_S %in% 226] = '12'
Pulse15$Day [Pulse15$DOY_S %in% 227] = '13'
Pulse15$Day [Pulse15$DOY_S %in% 228] = '14'


Pulse16$Day [Pulse16$DOY_S %in% 215] = '-7'
Pulse16$Day [Pulse16$DOY_S %in% 216] = '-6'
Pulse16$Day [Pulse16$DOY_S %in% 217] = '-5'
Pulse16$Day [Pulse16$DOY_S %in% 218] = '-4'
Pulse16$Day [Pulse16$DOY_S %in% 219] = '-3'
Pulse16$Day [Pulse16$DOY_S %in% 220] = '-2'
Pulse16$Day [Pulse16$DOY_S %in% 221] = '-1'
Pulse16$Day [Pulse16$DOY_S %in% 222] = '0'
Pulse16$Day [Pulse16$DOY_S %in% 223] = '1'
Pulse16$Day [Pulse16$DOY_S %in% 224] = '2'
Pulse16$Day [Pulse16$DOY_S %in% 225] = '3'
Pulse16$Day [Pulse16$DOY_S %in% 226] = '4'
Pulse16$Day [Pulse16$DOY_S %in% 227] = '5'
Pulse16$Day [Pulse16$DOY_S %in% 228] = '6'
Pulse16$Day [Pulse16$DOY_S %in% 229] = '7'
Pulse16$Day [Pulse16$DOY_S %in% 230] = '8'
Pulse16$Day [Pulse16$DOY_S %in% 231] = '9'
Pulse16$Day [Pulse16$DOY_S %in% 232] = '10'
Pulse16$Day [Pulse16$DOY_S %in% 233] = '11'
Pulse16$Day [Pulse16$DOY_S %in% 234] = '12'
Pulse16$Day [Pulse16$DOY_S %in% 235] = '13'
Pulse16$Day [Pulse16$DOY_S %in% 236] = '14'


Pulse17$Day [Pulse17$DOY_S %in% 217] = '-7'
Pulse17$Day [Pulse17$DOY_S %in% 218] = '-6'
Pulse17$Day [Pulse17$DOY_S %in% 219] = '-5'
Pulse17$Day [Pulse17$DOY_S %in% 220] = '-4'
Pulse17$Day [Pulse17$DOY_S %in% 221] = '-3'
Pulse17$Day [Pulse17$DOY_S %in% 222] = '-2'
Pulse17$Day [Pulse17$DOY_S %in% 223] = '-1'
Pulse17$Day [Pulse17$DOY_S %in% 224] = '0'
Pulse17$Day [Pulse17$DOY_S %in% 225] = '1'
Pulse17$Day [Pulse17$DOY_S %in% 226] = '2'
Pulse17$Day [Pulse17$DOY_S %in% 227] = '3'
Pulse17$Day [Pulse17$DOY_S %in% 228] = '4'
Pulse17$Day [Pulse17$DOY_S %in% 229] = '5'
Pulse17$Day [Pulse17$DOY_S %in% 230] = '6'
Pulse17$Day [Pulse17$DOY_S %in% 231] = '7'
Pulse17$Day [Pulse17$DOY_S %in% 232] = '8'
Pulse17$Day [Pulse17$DOY_S %in% 233] = '9'
Pulse17$Day [Pulse17$DOY_S %in% 234] = '10'
Pulse17$Day [Pulse17$DOY_S %in% 235] = '11'
Pulse17$Day [Pulse17$DOY_S %in% 236] = '12'
Pulse17$Day [Pulse17$DOY_S %in% 237] = '13'
Pulse17$Day [Pulse17$DOY_S %in% 238] = '14'


Pulse18$Day [Pulse18$DOY_S %in% 344] = '-7'
Pulse18$Day [Pulse18$DOY_S %in% 345] = '-6'
Pulse18$Day [Pulse18$DOY_S %in% 346] = '-5'
Pulse18$Day [Pulse18$DOY_S %in% 347] = '-4'
Pulse18$Day [Pulse18$DOY_S %in% 348] = '-3'
Pulse18$Day [Pulse18$DOY_S %in% 349] = '-2'
Pulse18$Day [Pulse18$DOY_S %in% 350] = '-1'
Pulse18$Day [Pulse18$DOY_S %in% 351] = '0'
Pulse18$Day [Pulse18$DOY_S %in% 352] = '1'
Pulse18$Day [Pulse18$DOY_S %in% 353] = '2'
Pulse18$Day [Pulse18$DOY_S %in% 354] = '3'
Pulse18$Day [Pulse18$DOY_S %in% 355] = '4'
Pulse18$Day [Pulse18$DOY_S %in% 356] = '5'
Pulse18$Day [Pulse18$DOY_S %in% 357] = '6'
Pulse18$Day [Pulse18$DOY_S %in% 358] = '7'
Pulse18$Day [Pulse18$DOY_S %in% 359] = '8'
Pulse18$Day [Pulse18$DOY_S %in% 360] = '9'
Pulse18$Day [Pulse18$DOY_S %in% 361] = '10'
Pulse18$Day [Pulse18$DOY_S %in% 362] = '11'
Pulse18$Day [Pulse18$DOY_S %in% 363] = '12'
Pulse18$Day [Pulse18$DOY_S %in% 364] = '13'
Pulse18$Day [Pulse18$DOY_S %in% 365] = '14'


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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))


Pulse5_sum <- Pulse5_pro %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr::summarise(meanAT2=mean(AT2,na.rm=TRUE), 
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
                   meanNEE=mean(as.numeric(NEE), na.rm=TRUE), 
                   meanGPP=mean(as.numeric(GPP), na.rm=TRUE),
                   meanRECO=mean(as.numeric(RECO), na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))


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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))

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
                    meanRECO=mean(RECO, na.rm=TRUE), meanday=mean(as.numeric(Day), na.rm= TRUE))


# Create the "Pulse" column for each Pulse(n)_sum
Pulse1_sum$Pulse <- 1
Pulse2_sum$Pulse <- 2
Pulse3_sum$Pulse <- 3
Pulse4_sum$Pulse <- 4
Pulse5_sum$Pulse <- 5
Pulse6_sum$Pulse <- 6
Pulse7_sum$Pulse <- 7
Pulse8_sum$Pulse <- 8
Pulse9_sum$Pulse <- 9
Pulse10_sum$Pulse <- 10
Pulse11_sum$Pulse <- 11
Pulse12_sum$Pulse <- 12
Pulse13_sum$Pulse <- 13
Pulse14_sum$Pulse <- 14
Pulse15_sum$Pulse <- 15
Pulse16_sum$Pulse <- 16
Pulse17_sum$Pulse <- 17
Pulse18_sum$Pulse <- 18


#Next step is to merge the all data frames with "_sum" based on the same column in all 18 Pulses which is called "Pulse"
All_yearPulses <- dplyr::bind_rows (Pulse1_sum, Pulse2_sum, Pulse3_sum, Pulse4_sum,
                                    Pulse5_sum, Pulse6_sum, Pulse7_sum, Pulse8_sum,
                                    Pulse9_sum, Pulse10_sum, Pulse11_sum, Pulse12_sum,
                                    Pulse13_sum, Pulse14_sum, Pulse15_sum, Pulse16_sum,
                                    Pulse17_sum, Pulse18_sum)

plot(All_yearPulses$meanRECO)


All_yearPulses %>%
  group_by(Pulse) %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  dplyr:: summarise(sum(meanRECO))

All_yearPulses %>%
  na.omit() %>%
  group_by(Pulse) %>%
  ggplot(aes(x= `as.numeric(DOY_S)`, y = meanRECO))+
  geom_line()+
  geom_point()+
  xlab('DOY')+
  ylab('Mean Reco (micromol m-2 s-1)')
  
All_yearPulses %>%
  na.omit() %>%
  group_by(Pulse) %>%
  filter(meanday>=0) %>%
  ggplot(aes(x= `as.numeric(DOY_S)`, y = meanRECO))+
  geom_line()+
  geom_point()+
  xlab('DOY')+
  ylab('Mean Reco (micromol m-2 s-1)')


All_yearPulses %>%
  na.omit() %>%
  filter(Pulse==1) %>%
  filter(meanday>0) %>%
  ggplot(aes(x= meanday, y = meanRECO))+
  geom_line()+
  geom_point()+
  xlab('Day after the Pulse')+
  ylab('Mean Reco (micromol m-2 s-1)')


All_yearPulses %>%
  na.omit() %>%
  filter(Pulse==18) %>%
  filter(meanday %in% -3:14) %>%
  ggplot(aes(x= meanday, y = meanRECO))+
  geom_line()+
  geom_point()+
  xlab('Day after the Pulse')+
  ylab('Mean Reco (micromol m-2 s-1)')+
  theme_classic()
 


# Calculate cumulative fluxes for each day and them all together
# First take Pulse 1

Pulse1_sum$DayFlux <- (Pulse1_sum$meanRECO*60*60*24)/1000000
sum(Pulse1_sum$DayFlux)
plot(Pulse1_sum$DayFlux)
Pulse1_sum$CumulFlux <- cumsum(Pulse1_sum$DayFlux)
plot(Pulse1_sum$`as.numeric(DOY_S)`, Pulse1_sum$CumulFlux)
Pulse1_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse2_sum$DayFlux <- (Pulse2_sum$meanRECO*60*60*24)/1000000
Pulse2_sum$CumulFlux <- cumsum(Pulse2_sum$DayFlux)
Pulse2_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse3_sum$DayFlux <- (Pulse3_sum$meanRECO*60*60*24)/1000000
Pulse3_sum$CumulFlux <- cumsum(Pulse3_sum$DayFlux)
Pulse3_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse4_sum$DayFlux <- (Pulse4_sum$meanRECO*60*60*24)/1000000
Pulse4_sum$CumulFlux <- cumsum(Pulse4_sum$DayFlux)
Pulse4_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse5_sum$DayFlux <- (Pulse5_sum$meanRECO*60*60*24)/1000000
Pulse5_sum$CumulFlux <- cumsum(Pulse5_sum$DayFlux)
Pulse5_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse6_sum$DayFlux <- (Pulse6_sum$meanRECO*60*60*24)/1000000
Pulse6_sum$CumulFlux <- cumsum(Pulse6_sum$DayFlux)
Pulse6_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse7_sum$DayFlux <- (Pulse7_sum$meanRECO*60*60*24)/1000000
Pulse7_sum$CumulFlux <- cumsum(Pulse7_sum$DayFlux)
Pulse7_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse8_sum$DayFlux <- (Pulse8_sum$meanRECO*60*60*24)/1000000
Pulse8_sum$CumulFlux <- cumsum(Pulse8_sum$DayFlux)
Pulse8_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse9_sum$DayFlux <- (Pulse9_sum$meanRECO*60*60*24)/1000000
Pulse9_sum$CumulFlux <- cumsum(Pulse9_sum$DayFlux)
Pulse9_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse10_sum$DayFlux <- (Pulse10_sum$meanRECO*60*60*24)/1000000
Pulse10_sum$CumulFlux <- cumsum(Pulse10_sum$DayFlux)
Pulse10_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse11_sum$DayFlux <- (Pulse11_sum$meanRECO*60*60*24)/1000000
Pulse11_sum$CumulFlux <- cumsum(Pulse11_sum$DayFlux)
Pulse11_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse12_sum$DayFlux <- (Pulse12_sum$meanRECO*60*60*24)/1000000
Pulse12_sum$CumulFlux <- cumsum(Pulse12_sum$DayFlux)
Pulse12_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse13_sum$DayFlux <- (Pulse13_sum$meanRECO*60*60*24)/1000000
Pulse13_sum$CumulFlux <- cumsum(Pulse13_sum$DayFlux)
Pulse13_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse14_sum$DayFlux <- (Pulse14_sum$meanRECO*60*60*24)/1000000
Pulse14_sum$CumulFlux <- cumsum(Pulse14_sum$DayFlux)
Pulse14_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse15_sum$DayFlux <- (Pulse15_sum$meanRECO*60*60*24)/1000000
Pulse15_sum$CumulFlux <- cumsum(Pulse15_sum$DayFlux)
Pulse15_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse16_sum$DayFlux <- (Pulse16_sum$meanRECO*60*60*24)/1000000
Pulse16_sum$CumulFlux <- cumsum(Pulse16_sum$DayFlux)
Pulse16_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse17_sum$DayFlux <- (Pulse17_sum$meanRECO*60*60*24)/1000000
Pulse17_sum$CumulFlux <- cumsum(Pulse17_sum$DayFlux)
Pulse17_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))

Pulse18_sum$DayFlux <- (Pulse18_sum$meanRECO*60*60*24)/1000000
Pulse18_sum$CumulFlux <- cumsum(Pulse18_sum$DayFlux)
Pulse18_sum %>%
  filter(as.numeric(meanday) %in% 0:14) %>%
  summarise(sum=sum(CumulFlux))


# Make a new_All file with 18-rows
All_yearPulses <- All_yearPulses [, c(18, 17, 1:16, 19, 20)]
All_yearPulses[All_yearPulses == -9999] <- NA

All_yearPulses_new <- All_yearPulses %>%
  filter(meanday %in% (-1:1))

  
All_yearPulses_new1 <- All_yearPulses_new %>%
  pivot_longer(names_to = "Factor",
              values_to = "Value", 
              'meanAT2':'DayFlux')


All_yearPulses_new2 <- subset(All_yearPulses_new1, select = -c (4))


All_yearPulses_new3 <- All_yearPulses_new2 %>%
  pivot_wider(names_from = Factor,
               values_from = Value)


All_yearPulses_Init <- All_yearPulses %>%
  filter(meanday == -1)

All_yearPulses_1 <- All_yearPulses %>%
  filter(meanday == 1)

All_yearPulses_0 <- All_yearPulses %>%
  filter(meanday == 0) %>%
  cbind(All_yearPulses_Init, meanAT2_I = All_yearPulses_Init$meanAT2,
        meanAT6_I=All_yearPulses_Init$meanAT6, sum_R_I = All_yearPulses_Init$sum_R, 
        rain_event_I = All_yearPulses_Init$rain_events,
        meanRH2_I = All_yearPulses_Init$meanRH2, meanRH6_I = All_yearPulses_Init$meanRH6, 
        meanSWC5_I = All_yearPulses_Init$meanSWC5, meanSWC15_I = All_yearPulses_Init$meanSWC15, 
        meanSWC30_I = All_yearPulses_Init$meanSWC30, meanST5_I = All_yearPulses_Init$meanST5, 
        meanST15_I = All_yearPulses_Init$meanST15, meanST30_I = All_yearPulses_Init$meanST30,
        meanNEE_I = All_yearPulses_Init$meanNEE, meanGPP_I = All_yearPulses_Init$meanGPP, 
        meanRECO_I = All_yearPulses_Init$meanRECO, DayFlux_I = All_yearPulses_Init$DayFlux,
        )


#################### Now this data set includes the initial conditions of each Pulse (-1 day) 
#and the condition after (+1 day) for all range of variables 

All_yearPulses_0 <- All_yearPulses %>%
  filter(meanday == 0) %>%
  cbind(meanAT2_I = All_yearPulses_Init$meanAT2,
        meanAT6_I=All_yearPulses_Init$meanAT6, sum_R_I = All_yearPulses_Init$sum_R, 
        rain_event_I = All_yearPulses_Init$rain_events,
        meanRH2_I = All_yearPulses_Init$meanRH2, meanRH6_I = All_yearPulses_Init$meanRH6, 
        meanSWC5_I = All_yearPulses_Init$meanSWC5, meanSWC15_I = All_yearPulses_Init$meanSWC15, 
        meanSWC30_I = All_yearPulses_Init$meanSWC30, meanST5_I = All_yearPulses_Init$meanST5, 
        meanST15_I = All_yearPulses_Init$meanST15, meanST30_I = All_yearPulses_Init$meanST30,
        meanNEE_I = All_yearPulses_Init$meanNEE, meanGPP_I = All_yearPulses_Init$meanGPP, 
        meanRECO_I = All_yearPulses_Init$meanRECO, DayFlux_I = All_yearPulses_Init$DayFlux,
# Adding the right after conditions
        meanAT2_A = All_yearPulses_1$meanAT2,
        meanAT6_A=All_yearPulses_1$meanAT6, sum_R_A = All_yearPulses_1$sum_R, 
        rain_event_A = All_yearPulses_1$rain_events,
        meanRH2_A = All_yearPulses_1$meanRH2, meanRH6_A = All_yearPulses_1$meanRH6, 
        meanSWC5_A = All_yearPulses_1$meanSWC5, meanSWC15_A = All_yearPulses_1$meanSWC15, 
        meanSWC30_A = All_yearPulses_1$meanSWC30, meanST5_A = All_yearPulses_1$meanST5, 
        meanST15_A = All_yearPulses_1$meanST15, meanST30_A = All_yearPulses_1$meanST30,
        meanNEE_A = All_yearPulses_1$meanNEE, meanGPP_A = All_yearPulses_1$meanGPP, 
        meanRECO_A = All_yearPulses_1$meanRECO, DayFlux_A = All_yearPulses_1$DayFlux,
# Adding pick values
pickReco = Pulses_picks$pickReco, pickGPP = Pulses_picks$pickGPP, pickNEE = Pulses_picks$pickNEE
)
  


plot(All_yearPulses_0$meanAT2_I, All_yearPulses_0$meanRECO)


All_yearPulses_0 %>%
  na.omit() %>% 
  ggplot(aes(x= meanAT2_I, y = meanRECO, size= meanSWC5_I))+
  geom_point()+
  xlab('Initial temperature, C')+
  ylab('Mean Reco (micromol m-2 s-1)')+
  theme_classic()

All_yearPulses_0 %>%
  na.omit() %>% 
  ggplot(aes(x= meanGPP, y = meanRECO, size= meanSWC5_I))+
  geom_point()+
  xlab('GPP')+
  ylab('Reco (micromol m-2 s-1)')+
  theme_classic()




# Add Pick Flux column 
Pulse1_pick <- Pulse1 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse1_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
    )

Pulse2_pick <- Pulse2 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse2_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse3_pick <- Pulse3 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse3_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse4_pick <- Pulse4 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse4_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse5_pick <- Pulse5_pro %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse5_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse6_pick <- Pulse6 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse6_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse7_pick <- Pulse7 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse7_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse8_pick <- Pulse8 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse8_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse9_pick <- Pulse9 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse9_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse10_pick <- Pulse10 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse10_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse11_pick <- Pulse11 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse11_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse12_pick <- Pulse12 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse12_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse13_pick <- Pulse13 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse13_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse14_pick <- Pulse14 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse14_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse15_pick <- Pulse15 %>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse15_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse16_pick <- Pulse16 %>%
  na.omit()%>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse16_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse17_pick <- Pulse17 %>%
  na.omit()%>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse17_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )

Pulse18_pick <- Pulse18 %>%
  na.omit()%>%
  filter(Day %in% 0:1) %>%
  dplyr:: summarise(
    Pulse=mean(Pulse18_sum$Pulse),
    pickReco=mean(max(RECO)),
    pickNEE=mean(max(NEE)),
    pickGPP=mean(max(GPP))
  )


#### Make this procedure for all Pick-files
#Pulses_18rows <- dplyr::bind_rows (All_yearPulses_Init, All_yearPulses_0)

Pulses_picks <- dplyr::bind_rows (Pulse1_pick, Pulse2_pick, Pulse3_pick,
                                  Pulse4_pick, Pulse5_pick, Pulse6_pick,
                                  Pulse7_pick, Pulse8_pick, Pulse9_pick,
                                  Pulse10_pick, Pulse11_pick, Pulse12_pick,
                                  Pulse13_pick, Pulse14_pick, Pulse15_pick,
                                  Pulse16_pick, Pulse17_pick, Pulse18_pick)


######################### Add Pick-columns to the 18_rows doc - ++++ All_yearPulses_0 - RUN AGAIN

############ Plot graphs Including initial conditions and pick value

All_yearPulses_0 %>%
  na.omit() %>% 
  ggplot(aes(x= meanAT2_I, y = pickReco, size= meanSWC5_I))+
  geom_point()+
  xlab('Initial AT')+
  ylab('Reco (micromol m-2 s-1)')+
  theme_classic()


All_yearPulses_0 %>%
  na.omit() %>% 
  ggplot(aes(x= meanSWC5_I, y = pickReco, size= sum_R))+
  geom_point()+
  xlab('Initial SWC')+
  ylab('Reco (micromol m-2 s-1)')+
  theme_classic()+
  ggtitle('Reco pick')+
  stat_smooth(method = "lm")+
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))

All_yearPulses_0 %>%
  na.omit() %>% 
  ggplot(aes(x= meanST5_I, y = pickReco, size= sum_R))+
  geom_point()+
  xlab('Initial Soil temperature')+
  ylab('Pick Reco (micromol m-2 s-1)')+
  theme_classic()+
  ggtitle('Reco pick')+
  stat_smooth(method = "lm")+
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))


All_yearPulses_0 %>%
  na.omit() %>% 
  ggplot(aes(x= DayFlux_I, y = pickReco, size= sum_R))+
  geom_point()+
  xlab('Initial Daily Flux')+
  ylab('Pick Reco (micromol m-2 s-1)')+
  theme_classic()+
  ggtitle('Reco pick')+
  stat_smooth(method = "lm")+
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))




