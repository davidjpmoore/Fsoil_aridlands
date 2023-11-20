# Purpose: Process all the Kendall Grassland data and produce one file years_sum1 which contains


#packages we need
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)
library(PerformanceAnalytics)
library(xtable)
library(ggpubr)
library(ggplot2)
library(colorRamps)
library(reshape2)
library(zoo)

#read all AMF fluxes and meteo observations from US-Wkg 
USWkg12_18 <- read.csv("C:/Users/user/Documents/Data Fsoil/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv", header=TRUE, na.strings="NaN", skip=0)
USWkg18_19 <- read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv", header=TRUE, na.strings="NaN", skip=0)
USWkg19_20 <- read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv", header=TRUE, na.strings="NaN", skip=0)
  


USWkg12_18$T_CANOPY_2_1_1 <- as.numeric(USWkg12_18$T_CANOPY_2_1_1)
USWkg18_19$T_CANOPY_2_1_1 <- as.numeric(USWkg18_19$T_CANOPY_2_1_1)
USWkg19_20$T_CANOPY_2_1_1 <- as.numeric(USWkg19_20$T_CANOPY_2_1_1)
USWkg12_20 <- bind_rows(USWkg12_18, USWkg18_19, USWkg19_20)


#Replace -9999 with NA for all data
USWkg12_20[USWkg12_20 == -9999] <- NA

# 
# # Remove missing or invalid data
# USWkg12_20 <- USWkg12_20[!is.na(USWkg12_20$FC), ]


# Convert TIMESTAMP_START to POSIXct
USWkg12_20$TIMESTAMP_START <- ymd_hm(as.character(USWkg12_20$TIMESTAMP_START))

# Extract year, month, day of year, hour of day and minute
USWkg12_20$year <- format(USWkg12_20$TIMESTAMP_START, "%Y")
USWkg12_20$month <- format(USWkg12_20$TIMESTAMP_START, "%m")
USWkg12_20$day_of_year <- format(USWkg12_20$TIMESTAMP_START, "%j")
USWkg12_20$hour <- format(USWkg12_20$TIMESTAMP_START, "%H")
USWkg12_20$minute <- format(USWkg12_20$TIMESTAMP_START, "%M")
USWkg12_20$date <- as.Date(USWkg12_20$TIMESTAMP_START)
USWkg12_20$DOY_S <- as.numeric(paste(yday(USWkg12_20$date)))

USWkg12_20$RainEvent_0 <- as.numeric(USWkg12_20$P>0)
USWkg12_20$RainEvent_5 <- as.numeric(USWkg12_20$P>5)

plot(USWkg12_20$DOY_S,USWkg12_20$GPP)

USWkg12_20<-  rename(USWkg12_20, 
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


plot(USWkg12_20$GPP)

# Aggregate daily values of FC by Date
GPP_date <- aggregate(GPP ~ date, data = (USWkg12_20), FUN = mean, na.rm = TRUE)

# Plot yearly time series of FC
plot(GPP_date$date, GPP_date$GPP, type = "l", xlab = "Year", ylab = "FC")


USWkg12_20_summary <- USWkg12_20 %>%
  mutate(RainEvent_0 = as.numeric(RainEvent_0)) %>%
  group_by(date) %>%
  summarise(
    meanAT2 = mean(AT2, na.rm = TRUE),
    meanAT6 = mean(AT6, na.rm = TRUE),
    sum_R = sum(P, na.rm = TRUE),
    rain_events = sum(RainEvent_0, na.rm = TRUE),
    meanRH2 = mean(RH2, na.rm = TRUE),
    meanRH6 = mean(RH6, na.rm = TRUE),
    meanSWC5 = mean(SWC5, na.rm = TRUE),
    meanSWC15 = mean(SWC15, na.rm = TRUE),
    meanSWC30 = mean(SWC30, na.rm = TRUE),
    meanST5 = mean(ST5, na.rm = TRUE),
    meanST15 = mean(ST15, na.rm = TRUE),
    meanST30 = mean(ST30, na.rm = TRUE),
    meanNEE = mean(NEE, na.rm = TRUE),
    meanGPP = mean(GPP, na.rm = TRUE),
    meanRECO = mean(RECO, na.rm = TRUE),
    sdReco = sd(RECO, na.rm = TRUE)
  )





par(mfrow=c(4,1), mar=c(0,4,0,0), oma=c(5,5,5,5))

plot(USWkg12_20_summary$date, USWkg12_20_summary$meanGPP, xaxt="n", xlab="", ylab="GPP")
plot(USWkg12_20_summary$date, USWkg12_20_summary$meanRECO, xaxt="n", xlab="", ylab="RECO")
plot(USWkg12_20_summary$date, USWkg12_20_summary$meanNEE, xaxt="n", xlab="", ylab="NEE")
plot(USWkg12_20_summary$date, USWkg12_20_summary$meanST15, xlab="Time", ylab="ST15")


write_csv(USWkg12_20_summary, "data/USWkg12_20_summary.csv")
 

USWkg12_20_summary$bigR <- as.numeric(USWkg12_20_summary$sum_R>5)

USWkg12_20_summary$bigRmm <- as.numeric(USWkg12_20_summary$bigR)*as.numeric(USWkg12_20_summary$sum_R)

hist(USWkg12_20_summary$bigRmm) 

USWkg12_20_summary$DOY <- paste(yday(USWkg12_20_summary$date))

USWkg12_20_summary$observation <- 1:nrow(USWkg12_20_summary)

USW9sum <- USWkg12_20_summary%>%
  filter(bigRmm > 0)

USW9sum %>%
  #group_by(DOY) %>%
  arrange(date) %>%
  mutate(diff = observation - lag(observation, default = first(observation)))

USW9sum$diff <- USW9sum$observation - lag(USW9sum$observation, 
                                          default = first(USW9sum$observation))

hist(USW9sum$diff)
USW9sum %>%
  ggplot(aes(x=bigRmm))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Rain > 5 mm')+
  ylab('Frequency')+
  xlab('Rain, mm')+
  theme_gray()+
  theme(text = element_text(size = 12))


hist(USWkg12_20_summary$meanRECO)

######Make seasons #############

USWkg12_20_summary$Season = vector(mode = 'character', length = nrow(USWkg12_20_summary))
USWkg12_20_summary$Season[USWkg12_20_summary$DOY %in% c(1:59,305:366)] = 'Winter'
USWkg12_20_summary$Season[USWkg12_20_summary$DOY %in% 60:181] = 'Spring'
USWkg12_20_summary$Season[USWkg12_20_summary$DOY %in% 182:304] = 'Summer'


#### Pulse Seasons ####
USW9sum$Season = vector(mode = 'character', length = nrow(USW9sum))
USW9sum$Season[USW9sum$DOY %in% c(1:59,305:366)] = 'Winter'
USW9sum$Season[USW9sum$DOY %in% 60:181] = 'Spring'
USW9sum$Season[USW9sum$DOY %in% 182:304] = 'Summer'


Pulse_Win <- USW9sum %>%
  filter(Season == 'Winter')

hist(Pulse_Win$meanRECO)
Pulse_Win %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Winter pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

Pulse_Spr <- USW9sum %>%
  filter(Season == 'Spring')

hist(Pulse_Spr$meanRECO)
Pulse_Spr %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Spring pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

Pulse_Sum <- USW9sum %>%
  filter(Season == 'Summer')

hist(Pulse_Sum$meanRECO)
Pulse_Sum %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Summer pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))


########## Pulse behavior for each season #########

USWkg12_20_summary$year <- substr(USWkg12_20_summary$date, 1,4)

USWkg12_20_summary$year <- as.numeric(as.character(USWkg12_20_summary$year))


######Spring Pulses ##########
plot(Pulse_Spr$meanRECO)

Pulse1Spr <- USWkg12_20_summary %>%
  filter(year == 2014) %>%
  filter(DOY %in% (57:74))
plot(Pulse1Spr$meanRECO)

Pulse1Spr %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanRECO),size=2)+
  geom_point(aes(y=meanSWC5/20), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
        )+
  #theme_get()+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('DOY')+
  ggtitle('Spring pulse1')+
  scale_y_continuous(name="Reco",
                     sec.axis = sec_axis( trans=~.*20, name="SWC")
  )
  #theme(panel.background = element_blank())
  

Pulse2Spr <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (68:84))
plot(Pulse2Spr$meanRECO)

Pulse2Spr %>%
  ggplot(aes(x=DOY, y = meanRECO))+
  geom_point()+
  theme(text = element_text(size = 15))+
  theme_bw()+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('DOY')+
  ggtitle('Spring pulse2')

Pulse3Spr <- USWkg12_20_summary %>%
  filter(year == 2020) %>%
  filter(DOY %in% (65:82))
plot(Pulse3Spr$meanRECO)

Pulse3Spr %>%
  ggplot(aes(x=DOY, y = meanRECO))+
  geom_point()+
  theme(text = element_text(size = 15))+
  theme_bw()+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('DOY')+
  ggtitle('Spring pulse3')

Pulse4Spr <- USWkg12_20_summary %>%
  filter(year == 2018) %>%
  filter(DOY %in% (164:181))
plot(Pulse4Spr$meanRECO)

Pulse4Spr %>%
  ggplot(aes(x=DOY, y = meanRECO))+
  geom_point()+
  theme(text = element_text(size = 15))+
  theme_bw()+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('DOY')+
  ggtitle('Spring pulse4')


######### Summer Pulses ######
Pulse_Sum %>%
  ggplot(aes(x=sum_R, y = meanRECO))+
  geom_point()  

Pulse1Sum <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (181:198))
plot(Pulse1Sum$meanRECO)

Pulse3Sum %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanRECO),size=2)+
  geom_point(aes(y=meanSWC5/5), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  #theme_get()+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('DOY')+
  ggtitle('Summer pulse1')+
  scale_y_continuous(name="Reco",
                     sec.axis = sec_axis( trans=~.*5, name="SWC")
  )

Pulse2Sum <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (196:213))
plot(Pulse2Sum$meanRECO)

Pulse3Sum <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (237:254))
plot(Pulse3Sum$meanRECO)

########## Winter Pulses ##########
plot(Pulse_Win$meanRECO)

Pulse1Win <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (11:28))
plot(Pulse1Win$meanRECO)


Pulse2Win %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanRECO),size=2)+
  geom_point(aes(y=meanSWC5/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  #theme_get()+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('DOY')+
  ggtitle('Winter pulse1')+
  scale_y_continuous(name="Reco",
                     sec.axis = sec_axis( trans=~.*10, name="SWC")
  )


Pulse2Win <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (348:365))
plot(Pulse2Win$meanRECO)

Pulse3Win <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (47:64))
plot(Pulse3Win$meanRECO)

##### plot graphs with Reco and GPP for pulse and non-pulse time and together ####
USWkg12_20_summary %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape = 1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('All data')

####### Make 2 more ##########

#Pulses_2020_n <- summary2020_all %>%
 # filter(sum_R > 5)

#colnames(summary2020_all)[1]<-'DOY'
#test <- data.frame(DOY=Pulses_2020_n$DOY_S)
#test$day1<-test$DOY+1
#test$day2<-test$DOY+2
#test$day3<-test$DOY+3
#test$day4<-test$DOY+4
#test2<-melt(test)
#test2<-data.frame(DOY=unique(test2$value))
#test2$Pulse_DOY <- test2$DOY
#summary2020_new <- merge(summary2020_all,test2,by="DOY",all.x=TRUE)


USWkg12_20_summary$DOY <- as.numeric(as.character(USWkg12_20_summary$DOY))
USWkg12_20_summary$Pulse_DOY <- USWkg12_20_summary$DOY*USWkg12_20_summary$bigR


USWkg12_20_summary$data <- USWkg12_20_summary$DOY
USWkg12_20_summary <- USWkg12_20_summary[,-c(1)]

USW1220_Pulse <- USWkg12_20_summary %>%
  filter(bigRmm > 0)

write.csv(USW1220_Pulse, "USW1220Pulse.csv")

USW1220_Pulse <- read.csv("USW1220Pulse.csv")
USW1220_PulseNon <- read.csv("USW1220PulseNon.csv")
USWkg12_20_summary <- read.csv("data/USWkg12_20_summary.csv")


USW1220_PulseNon <- USWkg12_20_summary %>%
  filter(bigRmm == 0)  

write.csv(USW1220_PulseNon, "USW1220PulseNon.csv")


USW1220_Pulse$DOY <- as.numeric(as.character(USW1220_Pulse$DOY))

colnames(USWkg12_20_summary) [1] <- 'DOY'
test <- data.frame(DOY=USW1220_Pulse$DOY)
test$day1<-test$DOY+1
test$day2<-test$DOY+2
test$day3<-test$DOY+3
test$day4<-test$DOY+4
test$date <- USW1220_Pulse$date
test2<- melt(test, id='date')
test3 <- test2 %>% 
  arrange(date)
test3$Day<- as.Date(test3$value-1,  origin = "2013-01-01")

colnames(USWkg12_20_summary) [1] <- 'date'
test <- data.frame(DOY=USW1220_Pulse$date)
test$day1<-as.Date(test$DOY)+1
test$day2<-as.Date(test$DOY)+2
test$day3<-as.Date(test$DOY)+3
test$day4<-as.Date(test$DOY)+4
test$Sday <- yday(test$DOY)
test2<- melt(test,id='Sday')
test3 <- test2 %>% 
  arrange(value)
test3 <- data.frame(unique(test2$value))
test3$date <- test3$unique.test2.value.
test4 <- test3 %>% 
  arrange(date)

test4$DOY <- yday(test4$date)
test4 <- test4[-c(1)]

#USWkg12_20_summary$DOY <- USWkg12_20_summary$date
#USWkg12_20_summary <- USWkg12_20_summary[-c(1)]
#USWkg12_20_summary$meanAT2 <- USWkg12_20_summary$date

summary_P <- merge(USWkg12_20_summary,test4,by="date",all.x=TRUE)

summary_P$PulseDays <- summary_P$DOY.y
summary_P$PulseDays <- as.numeric(as.integer(summary_P$PulseDays))
 
USW_Pulse <- summary_P %>%
  filter(PulseDays > 0)

write.csv(USW_Pulse, "data/USWPulse.csv")

summary_P$PulseDays[is.na(summary_P$PulseDays)] <- 0

write.csv(summary_P, "data/Summary_eddy.csv")

USW_PulseN <- summary_P %>%
  filter(PulseDays == 0)

write.csv(USW_PulseN, "data/USWPulseN.csv")



summary_P %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('All time')



USWkg12_20_summary$data <- USWkg12_20_summary$DOY
USWkg12_20_summary <- USWkg12_20_summary[,-c(1)]

USW1220_Pulse <- USWkg12_20_summary %>%
  filter(bigRmm > 0)

write.csv(USW1220_Pulse, "USW1220Pulse.csv")

USW1220_PulseNon <- USWkg12_20_summary %>%
  filter(bigRmm == 0)  

write.csv(USW1220_PulseNon, "USW1220PulseNon.csv")

USW1220_Pulse %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Pulse time')


USW1220_PulseNon %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Non-pulse time')

#### Next part - JUST Reading previous files ############
USW_Pulse <- read.csv("data/USWPulse.csv")
summary_P <- read.csv("data/Summary_eddy.csv")
USW_PulseN <- read.csv("data/USWPulseN.csv")


USW_Pulse %>%
  #filter(RainEvent==1)%>%
  ggplot(aes(x= meanSWC5, y= meanST5, size = bigRmm, color = Season)) + 
  geom_point()+
  theme_bw()+
  theme(text = element_text(size = 15))+
  xlab('Mean SWC, 5 cm')+
  ylab('Mean Soil temperature, 5 cm')

USW_Pulse %>%
  #filter(RainEvent==1)%>%
  ggplot(aes(x= meanSWC5, y= meanST5, size = meanRECO, color = Season)) + 
  geom_point()+
  theme_test()+
  theme(text = element_text(size = 15))+
  xlab('Mean SWC, 5 cm')+
  ylab('Mean Soil temperature, 5 cm')+
  xlim(0,40) +ylim(0,40)


USW_PulseN %>%
  #filter(RainEvent==1)%>%
  ggplot(aes(x= meanSWC5, y= meanST5, size = meanRECO, color = Season)) + 
  geom_point()+
  theme_test()+
  theme(text = element_text(size = 15))+
  xlab('Mean SWC, 5 cm')+
  ylab('Mean Soil temperature, 5 cm')+
  xlim(0,40) +ylim(0,40)






