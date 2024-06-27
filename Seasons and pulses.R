# 13-06-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(matrixStats)



# In the next block we will create figures for pulses for 3 seasons ###
# Spring, Summer and Winter pulses ####################################

### We need 2 dfs - USW9sum and USWkg12_20_summary

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

# Pulses with Max CO2 release (more than 0.6 micromol m-2 s-1)
Pulse_Win_Max <- Pulse_Win %>%
  filter(meanRECO > 0.6)

# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxWin1 <- USWkg12_20_summary %>%
  filter(year == 2013) %>%
  filter(DOY %in% (323:340))

MaxWin2 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (316:333))

MaxWin3 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (343:360))

MaxWin4 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (29:46))

# Just not enough days in one year to finish this pulse
MaxWin5 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (354:366))

MaxWin6 <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (348:365))

# The next two pulses may overlap each other
MaxWin7 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (320:337))

MaxWin8 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (322:339))

MaxWin9 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (340:357))


# To exclude MaxWin5 and make the one df for 8 Reco and SWC5
MaxWin <- MaxWin1 %>%
  select(meanRECO, sdReco, meanSWC5,sdSWC5)

MaxWin$Reco2 <- MaxWin2$meanRECO
MaxWin$Reco2sd <- MaxWin2$sdReco
MaxWin$SWC2 <- MaxWin2$meanSWC5
MaxWin$SWC2sd <- MaxWin2$sdSWC5

MaxWin$Reco3 <- MaxWin3$meanRECO
MaxWin$Reco3sd <- MaxWin3$sdReco
MaxWin$SWC3 <- MaxWin3$meanSWC5
MaxWin$SWC3sd <- MaxWin3$sdSWC5

MaxWin$Reco4 <- MaxWin4$meanRECO
MaxWin$Reco4sd <- MaxWin4$sdReco
MaxWin$SWC4 <- MaxWin4$meanSWC5
MaxWin$SWC4sd <- MaxWin4$sdSWC5

MaxWin$Reco5 <- MaxWin6$meanRECO
MaxWin$Reco5sd <- MaxWin6$sdReco
MaxWin$SWC5 <- MaxWin6$meanSWC5
MaxWin$SWC5sd <- MaxWin6$sdSWC5

MaxWin$Reco6 <- MaxWin7$meanRECO
MaxWin$Reco6sd <- MaxWin7$sdReco
MaxWin$SWC6 <- MaxWin7$meanSWC5
MaxWin$SWC6sd <- MaxWin7$sdSWC5

MaxWin$Reco7 <- MaxWin8$meanRECO
MaxWin$Reco7sd <- MaxWin8$sdReco
MaxWin$SWC7 <- MaxWin8$meanSWC5
MaxWin$SWC7sd <- MaxWin8$sdSWC5

MaxWin$Reco8 <- MaxWin9$meanRECO
MaxWin$Reco8sd <- MaxWin9$sdReco
MaxWin$SWC8 <- MaxWin9$meanSWC5
MaxWin$SWC8sd <- MaxWin9$sdSWC5

# make them numeric
MaxWin$meanRECO <- as.numeric(as.character(MaxWin$meanRECO))
MaxWin$Reco2 <- as.numeric(as.character(MaxWin$Reco2))
MaxWin$Reco3 <- as.numeric(as.character(MaxWin$Reco3))
MaxWin$Reco4 <- as.numeric(as.character(MaxWin$Reco4))
MaxWin$Reco5 <- as.numeric(as.character(MaxWin$Reco5))
MaxWin$Reco6 <- as.numeric(as.character(MaxWin$Reco6))
MaxWin$Reco7 <- as.numeric(as.character(MaxWin$Reco7))
MaxWin$Reco8 <- as.numeric(as.character(MaxWin$Reco8))

MaxWin$meanSR <- rowMeans(MaxWin[,c(1,5,9,13,17,21,25,29)], na.rm=TRUE)

#MaxWin$meanSRsd <- rowSds(MaxWin[,c(1,5,9,13,17,21,25,29)], na.rm=TRUE)

MaxWin$SD_SR = vector(mode = 'character', length = nrow(MaxWin))
MaxWin$SD_SR[MaxWin$DOY %in% 323] = 0.2
MaxWin$SD_SR[MaxWin$DOY %in% 324] = 0.26
MaxWin$SD_SR[MaxWin$DOY %in% 325] = 0.33
MaxWin$SD_SR[MaxWin$DOY %in% 326] = 0.08
MaxWin$SD_SR[MaxWin$DOY %in% 327] = 0.19
MaxWin$SD_SR[MaxWin$DOY %in% 328] = 0.18
MaxWin$SD_SR[MaxWin$DOY %in% 329] = 0.16
MaxWin$SD_SR[MaxWin$DOY %in% 330] = 0.18
MaxWin$SD_SR[MaxWin$DOY %in% 331] = 0.21
MaxWin$SD_SR[MaxWin$DOY %in% 332] = 0.19
MaxWin$SD_SR[MaxWin$DOY %in% 333] = 0.16
MaxWin$SD_SR[MaxWin$DOY %in% 334] = 0.19
MaxWin$SD_SR[MaxWin$DOY %in% 335] = 0.3
MaxWin$SD_SR[MaxWin$DOY %in% 336] = 0.21
MaxWin$SD_SR[MaxWin$DOY %in% 337] = 0.18
MaxWin$SD_SR[MaxWin$DOY %in% 338] = 0.17
MaxWin$SD_SR[MaxWin$DOY %in% 339] = 0.11
MaxWin$SD_SR[MaxWin$DOY %in% 340] = 0.11

MaxWin$SD_SR <- as.numeric(as.character(MaxWin$SD_SR))


MaxWin$meanSWC5 <- as.numeric(as.character(MaxWin$meanSWC5))
MaxWin$SWC2 <- as.numeric(as.character(MaxWin$SWC2))
MaxWin$SWC3 <- as.numeric(as.character(MaxWin$SWC3))
MaxWin$SWC4 <- as.numeric(as.character(MaxWin$SWC4))
MaxWin$SWC5 <- as.numeric(as.character(MaxWin$SWC5))
MaxWin$SWC6 <- as.numeric(as.character(MaxWin$SWC6))
MaxWin$SWC7 <- as.numeric(as.character(MaxWin$SWC7))
MaxWin$SWC8 <- as.numeric(as.character(MaxWin$SWC8))

MaxWin$meanSWC <- rowMeans(MaxWin[,c(3,7,11,15,19,23,27,31)], na.rm=TRUE)

MaxWin$DOY <- MaxWin1$DOY 

MaxWin$SD_SWC = vector(mode = 'character', length = nrow(MaxWin))
MaxWin$SD_SWC[MaxWin$DOY %in% 323] = 4.35
MaxWin$SD_SWC[MaxWin$DOY %in% 324] = 4.08
MaxWin$SD_SWC[MaxWin$DOY %in% 325] = 4.76
MaxWin$SD_SWC[MaxWin$DOY %in% 326] = 7.49
MaxWin$SD_SWC[MaxWin$DOY %in% 327] = 4.54
MaxWin$SD_SWC[MaxWin$DOY %in% 328] = 3.72
MaxWin$SD_SWC[MaxWin$DOY %in% 329] = 2.49
MaxWin$SD_SWC[MaxWin$DOY %in% 330] = 2.08
MaxWin$SD_SWC[MaxWin$DOY %in% 331] = 1.88
MaxWin$SD_SWC[MaxWin$DOY %in% 332] = 3.53
MaxWin$SD_SWC[MaxWin$DOY %in% 333] = 4.19
MaxWin$SD_SWC[MaxWin$DOY %in% 334] = 4.97
MaxWin$SD_SWC[MaxWin$DOY %in% 335] = 4.8
MaxWin$SD_SWC[MaxWin$DOY %in% 336] = 4.63
MaxWin$SD_SWC[MaxWin$DOY %in% 337] = 3.48
MaxWin$SD_SWC[MaxWin$DOY %in% 338] = 3.04
MaxWin$SD_SWC[MaxWin$DOY %in% 339] = 2.92
MaxWin$SD_SWC[MaxWin$DOY %in% 340] = 2.66

MaxWin$SD_SWC <- as.numeric(as.character(MaxWin$SD_SWC))


MaxWin$Pulse_day = vector(mode = 'character', length = nrow(MaxWin))
MaxWin$Pulse_day[MaxWin$DOY %in% 323] = '-3'
MaxWin$Pulse_day[MaxWin$DOY %in% 324] = '-2'
MaxWin$Pulse_day[MaxWin$DOY %in% 325] = '-1'
MaxWin$Pulse_day[MaxWin$DOY %in% 326] = '0'
MaxWin$Pulse_day[MaxWin$DOY %in% 327] = '1'
MaxWin$Pulse_day[MaxWin$DOY %in% 328] = '2'
MaxWin$Pulse_day[MaxWin$DOY %in% 329] = '3'
MaxWin$Pulse_day[MaxWin$DOY %in% 330] = '4'
MaxWin$Pulse_day[MaxWin$DOY %in% 331] = '5'
MaxWin$Pulse_day[MaxWin$DOY %in% 332] = '6'
MaxWin$Pulse_day[MaxWin$DOY %in% 333] = '7'
MaxWin$Pulse_day[MaxWin$DOY %in% 334] = '8'
MaxWin$Pulse_day[MaxWin$DOY %in% 335] = '9'
MaxWin$Pulse_day[MaxWin$DOY %in% 336] = '10'
MaxWin$Pulse_day[MaxWin$DOY %in% 337] = '11'
MaxWin$Pulse_day[MaxWin$DOY %in% 338] = '12'
MaxWin$Pulse_day[MaxWin$DOY %in% 339] = '13'
MaxWin$Pulse_day[MaxWin$DOY %in% 340] = '14'



################# Graph with Standard Errors!!!! #####################
MaxWin %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanSR),size=2)+
  geom_point(aes(y=meanSWC/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanSR - (SD_SR/sqrt(8)), ymax= meanSR + (SD_SR/sqrt(8))), width=.2,
                  position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/10 - ((SD_SWC/10)/sqrt(8)), ymax= meanSWC/10 + ((SD_SWC/10)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('DOY')+
  ggtitle('Mean Winter pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))

MaxWin$Pulse_day <- as.numeric(as.character(MaxWin$Pulse_day))

MaxWin %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanSR),size=2)+
  geom_point(aes(y=meanSWC/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanSR - (SD_SR/sqrt(8)), ymax= meanSR + (SD_SR/sqrt(8))), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/10 - ((SD_SWC/10)/sqrt(8)), ymax= meanSWC/10 + ((SD_SWC/10)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('Pulse duration, days')+
  #xlim(-3,14)+
  ggtitle('Mean Winter pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))


# Another classification - use the biggest rain events - more than 20 mm  


# Spring pulses
### We need 2 dfs - USW9sum and USWkg12_20_summary

Pulse_Spr <- USW9sum %>%
  filter(Season == 'Spring')

Pulse_Spr %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Spring pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

# Pulses with Max CO2 release (more than 0.6 micromol m-2 s-1)
Pulse_Spr_Max <- Pulse_Spr %>%
  filter(meanRECO > 1)

# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxSpr1 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (99:116))

MaxSpr2 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (174:191))

MaxSpr3 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (175:192))

MaxSpr4 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (178:195))

MaxSpr5 <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (173:190))

MaxSpr6 <- USWkg12_20_summary %>%
  filter(year == 2018) %>%
  filter(DOY %in% (164:181))

# Making one document 
MaxSpr <- MaxSpr1 %>%
  select(meanRECO, sdReco, meanSWC5,sdSWC5)

MaxSpr$Reco2 <- MaxSpr2$meanRECO
MaxSpr$Reco2sd <- MaxSpr2$sdReco
MaxSpr$SWC2 <- MaxSpr2$meanSWC5
MaxSpr$SWC2sd <- MaxSpr2$sdSWC5

MaxSpr$Reco3 <- MaxSpr3$meanRECO
MaxSpr$Reco3sd <- MaxSpr3$sdReco
MaxSpr$SWC3 <- MaxSpr3$meanSWC5
MaxSpr$SWC3sd <- MaxSpr3$sdSWC5

MaxSpr$Reco4 <- MaxSpr4$meanRECO
MaxSpr$Reco4sd <- MaxSpr4$sdReco
MaxSpr$SWC4 <- MaxSpr4$meanSWC5
MaxSpr$SWC4sd <- MaxSpr4$sdSWC5

MaxSpr$Reco5 <- MaxSpr5$meanRECO
MaxSpr$Reco5sd <- MaxSpr5$sdReco
MaxSpr$SWC5 <- MaxSpr5$meanSWC5
MaxSpr$SWC5sd <- MaxSpr5$sdSWC5

MaxSpr$Reco6 <- MaxSpr6$meanRECO
MaxSpr$Reco6sd <- MaxSpr6$sdReco
MaxSpr$SWC6 <- MaxSpr6$meanSWC5
MaxSpr$SWC6sd <- MaxSpr6$sdSWC5


MaxSpr$meanSR <- rowMeans(MaxSpr[,c(1,5,9,13,17,21)], na.rm=TRUE)
MaxSpr$meanSWC <- rowMeans(MaxSpr[,c(3,7,11,15,19,23)], na.rm=TRUE)

MaxSpr$DOY <- MaxSpr1$DOY 

MaxSpr$Pulse_day = vector(mode = 'character', length = nrow(MaxSpr))
MaxSpr$Pulse_day[MaxSpr$DOY %in% 99] = '-3'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 100] = '-2'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 101] = '-1'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 102] = '0'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 103] = '1'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 104] = '2'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 105] = '3'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 106] = '4'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 107] = '5'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 108] = '6'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 109] = '7'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 110] = '8'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 111] = '9'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 112] = '10'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 113] = '11'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 114] = '12'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 115] = '13'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 116] = '14'

MaxSpr$Pulse_day <- as.numeric(as.character(MaxSpr$Pulse_day))

MaxSpr$SD_SR = vector(mode = 'character', length = nrow(MaxSpr))
MaxSpr$SD_SR[MaxSpr$DOY %in% 99] = 0.26
MaxSpr$SD_SR[MaxSpr$DOY %in% 100] = 0.33
MaxSpr$SD_SR[MaxSpr$DOY %in% 101] = 0.54
MaxSpr$SD_SR[MaxSpr$DOY %in% 102] = 0.79
MaxSpr$SD_SR[MaxSpr$DOY %in% 103] = 0.69
MaxSpr$SD_SR[MaxSpr$DOY %in% 104] = 0.63
MaxSpr$SD_SR[MaxSpr$DOY %in% 105] = 0.43
MaxSpr$SD_SR[MaxSpr$DOY %in% 106] = 0.41
MaxSpr$SD_SR[MaxSpr$DOY %in% 107] = 0.68
MaxSpr$SD_SR[MaxSpr$DOY %in% 108] = 1.06
MaxSpr$SD_SR[MaxSpr$DOY %in% 109] = 1.27
MaxSpr$SD_SR[MaxSpr$DOY %in% 110] = 1.18
MaxSpr$SD_SR[MaxSpr$DOY %in% 111] = 0.99
MaxSpr$SD_SR[MaxSpr$DOY %in% 112] = 0.9
MaxSpr$SD_SR[MaxSpr$DOY %in% 113] = 0.78
MaxSpr$SD_SR[MaxSpr$DOY %in% 114] = 0.67
MaxSpr$SD_SR[MaxSpr$DOY %in% 115] = 0.72
MaxSpr$SD_SR[MaxSpr$DOY %in% 116] = 0.75

MaxSpr$SD_SR <- as.numeric(as.character(MaxSpr$SD_SR))

MaxSpr$SD_SWC = vector(mode = 'character', length = nrow(MaxSpr))
MaxSpr$SD_SWC[MaxSpr$DOY %in% 99] = 2.06
MaxSpr$SD_SWC[MaxSpr$DOY %in% 100] = 2.3
MaxSpr$SD_SWC[MaxSpr$DOY %in% 101] = 2.17
MaxSpr$SD_SWC[MaxSpr$DOY %in% 102] = 6.7
MaxSpr$SD_SWC[MaxSpr$DOY %in% 103] = 4.91
MaxSpr$SD_SWC[MaxSpr$DOY %in% 104] = 5.65
MaxSpr$SD_SWC[MaxSpr$DOY %in% 105] = 4.66
MaxSpr$SD_SWC[MaxSpr$DOY %in% 106] = 4.29
MaxSpr$SD_SWC[MaxSpr$DOY %in% 107] = 4
MaxSpr$SD_SWC[MaxSpr$DOY %in% 108] = 5.18
MaxSpr$SD_SWC[MaxSpr$DOY %in% 109] = 4.76
MaxSpr$SD_SWC[MaxSpr$DOY %in% 110] = 4.95
MaxSpr$SD_SWC[MaxSpr$DOY %in% 111] = 6.21
MaxSpr$SD_SWC[MaxSpr$DOY %in% 112] = 5.06
MaxSpr$SD_SWC[MaxSpr$DOY %in% 113] = 4.35
MaxSpr$SD_SWC[MaxSpr$DOY %in% 114] = 3.98
MaxSpr$SD_SWC[MaxSpr$DOY %in% 115] = 3.43
MaxSpr$SD_SWC[MaxSpr$DOY %in% 116] = 4.22

MaxSpr$SD_SWC <- as.numeric(as.character(MaxSpr$SD_SWC))

MaxSpr %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanSR),size=2)+
  geom_point(aes(y=meanSWC/15), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanSR - (SD_SR/sqrt(8)), ymax= meanSR + (SD_SR/sqrt(8))), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/15 - ((SD_SWC/15)/sqrt(8)), ymax= meanSWC/15 + ((SD_SWC/15)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('Pulse duration, days')+
  #ylim(0,4)+
  ggtitle('Mean Spring pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*15, 
                                          name="SWC 5 cm , %"))


# Summer pulses
Pulse_Sum <- USW9sum %>%
  filter(Season == 'Summer')

Pulse_Sum %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Summer pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

# Pulses with Max CO2 release (more than 0.6 micromol m-2 s-1)
Pulse_Sum_Max <- Pulse_Sum %>%
  filter(meanRECO > 2.7)

# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxSum1 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (181:198))

MaxSum2 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (183:200))

MaxSum3 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (209:226))

MaxSum4 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (196:213))

MaxSum5 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (197:214))

MaxSum6 <- USWkg12_20_summary %>%
  filter(year == 2018) %>%
  filter(DOY %in% (216:233))

MaxSum7 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (237:254))

# Making one document 
MaxSum <- MaxSum1 %>%
  select(meanRECO, sdReco, meanSWC5,sdSWC5)

MaxSum$Reco2 <- MaxSum2$meanRECO
MaxSum$Reco2sd <- MaxSum2$sdReco
MaxSum$SWC2 <- MaxSum2$meanSWC5
MaxSum$SWC2sd <- MaxSum2$sdSWC5

MaxSum$Reco3 <- MaxSum3$meanRECO
MaxSum$Reco3sd <- MaxSum3$sdReco
MaxSum$SWC3 <- MaxSum3$meanSWC5
MaxSum$SWC3sd <- MaxSpr3$sdSWC5

MaxSum$Reco4 <- MaxSum4$meanRECO
MaxSum$Reco4sd <- MaxSum4$sdReco
MaxSum$SWC4 <- MaxSum4$meanSWC5
MaxSum$SWC4sd <- MaxSum4$sdSWC5

MaxSum$Reco5 <- MaxSum5$meanRECO
MaxSum$Reco5sd <- MaxSum5$sdReco
MaxSum$SWC5 <- MaxSum5$meanSWC5
MaxSum$SWC5sd <- MaxSum5$sdSWC5

MaxSum$Reco6 <- MaxSum6$meanRECO
MaxSum$Reco6sd <- MaxSum6$sdReco
MaxSum$SWC6 <- MaxSum6$meanSWC5
MaxSum$SWC6sd <- MaxSum6$sdSWC5

MaxSum$Reco7 <- MaxSum7$meanRECO
MaxSum$Reco7sd <- MaxSum7$sdReco
MaxSum$SWC7 <- MaxSum7$meanSWC5
MaxSum$SWC7sd <- MaxSum7$sdSWC5

MaxSum$meanSR <- rowMeans(MaxSum[,c(1,5,9,13,17,21,25)], na.rm=TRUE)
MaxSum$meanSWC <- rowMeans(MaxSum[,c(3,7,11,15,19,23,27)], na.rm=TRUE)

MaxSum$DOY <- MaxSum1$DOY 

MaxSum$Pulse_day = vector(mode = 'character', length = nrow(MaxSum))
MaxSum$Pulse_day[MaxSum$DOY %in% 181] = '-3'
MaxSum$Pulse_day[MaxSum$DOY %in% 182] = '-2'
MaxSum$Pulse_day[MaxSum$DOY %in% 183] = '-1'
MaxSum$Pulse_day[MaxSum$DOY %in% 184] = '0'
MaxSum$Pulse_day[MaxSum$DOY %in% 185] = '1'
MaxSum$Pulse_day[MaxSum$DOY %in% 186] = '2'
MaxSum$Pulse_day[MaxSum$DOY %in% 187] = '3'
MaxSum$Pulse_day[MaxSum$DOY %in% 188] = '4'
MaxSum$Pulse_day[MaxSum$DOY %in% 189] = '5'
MaxSum$Pulse_day[MaxSum$DOY %in% 190] = '6'
MaxSum$Pulse_day[MaxSum$DOY %in% 191] = '7'
MaxSum$Pulse_day[MaxSum$DOY %in% 192] = '8'
MaxSum$Pulse_day[MaxSum$DOY %in% 193] = '9'
MaxSum$Pulse_day[MaxSum$DOY %in% 194] = '10'
MaxSum$Pulse_day[MaxSum$DOY %in% 195] = '11'
MaxSum$Pulse_day[MaxSum$DOY %in% 196] = '12'
MaxSum$Pulse_day[MaxSum$DOY %in% 197] = '13'
MaxSum$Pulse_day[MaxSum$DOY %in% 198] = '14'

MaxSum$Pulse_day <- as.numeric(as.character(MaxSum$Pulse_day))

MaxSum$SD_SR = vector(mode = 'character', length = nrow(MaxSum))
MaxSum$SD_SR[MaxSum$DOY %in% 181] = 0.36
MaxSum$SD_SR[MaxSum$DOY %in% 182] = 0.52
MaxSum$SD_SR[MaxSum$DOY %in% 183] = 0.39
MaxSum$SD_SR[MaxSum$DOY %in% 184] = 0.29
MaxSum$SD_SR[MaxSum$DOY %in% 185] = .45
MaxSum$SD_SR[MaxSum$DOY %in% 186] = .49
MaxSum$SD_SR[MaxSum$DOY %in% 187] = .54
MaxSum$SD_SR[MaxSum$DOY %in% 188] = .4
MaxSum$SD_SR[MaxSum$DOY %in% 189] = .36
MaxSum$SD_SR[MaxSum$DOY %in% 190] = .43
MaxSum$SD_SR[MaxSum$DOY %in% 191] = .51
MaxSum$SD_SR[MaxSum$DOY %in% 192] = .6
MaxSum$SD_SR[MaxSum$DOY %in% 193] = .58
MaxSum$SD_SR[MaxSum$DOY %in% 194] = .55
MaxSum$SD_SR[MaxSum$DOY %in% 195] = .55
MaxSum$SD_SR[MaxSum$DOY %in% 196] = 0.59
MaxSum$SD_SR[MaxSum$DOY %in% 197] = 0.5
MaxSum$SD_SR[MaxSum$DOY %in% 198] = .42

MaxSum$SD_SR <- as.numeric(as.character(MaxSum$SD_SR))

MaxSum$SD_SWC = vector(mode = 'character', length = nrow(MaxSum))
MaxSum$SD_SWC[MaxSum$DOY %in% 181] = 2.35
MaxSum$SD_SWC[MaxSum$DOY %in% 182] = 5.13
MaxSum$SD_SWC[MaxSum$DOY %in% 183] = 3.35
MaxSum$SD_SWC[MaxSum$DOY %in% 184] = 6.79
MaxSum$SD_SWC[MaxSum$DOY %in% 185] = 4.89
MaxSum$SD_SWC[MaxSum$DOY %in% 186] = 4.45
MaxSum$SD_SWC[MaxSum$DOY %in% 187] = 3.72
MaxSum$SD_SWC[MaxSum$DOY %in% 188] = 3.57
MaxSum$SD_SWC[MaxSum$DOY %in% 189] = 3.51
MaxSum$SD_SWC[MaxSum$DOY %in% 190] = 3.28
MaxSum$SD_SWC[MaxSum$DOY %in% 191] = 3.25
MaxSum$SD_SWC[MaxSum$DOY %in% 192] = 4.34
MaxSum$SD_SWC[MaxSum$DOY %in% 193] = 3.92
MaxSum$SD_SWC[MaxSum$DOY %in% 194] = 3.49
MaxSum$SD_SWC[MaxSum$DOY %in% 195] = 4.31
MaxSum$SD_SWC[MaxSum$DOY %in% 196] = 3.82
MaxSum$SD_SWC[MaxSum$DOY %in% 197] = 4.06
MaxSum$SD_SWC[MaxSum$DOY %in% 198] = 4.47

MaxSum$SD_SWC <- as.numeric(as.character(MaxSum$SD_SWC))

MaxSum %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanSR),size=2)+
  geom_point(aes(y=meanSWC/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanSR - (SD_SR/sqrt(8)), ymax= meanSR + (SD_SR/sqrt(8))), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/10 - ((SD_SWC/10)/sqrt(8)), ymax= meanSWC/10 + ((SD_SWC/10)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('Pulse duration, days')+
  #ylim(0,4)+
  ggtitle('Mean Summer pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))





