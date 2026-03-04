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
# Spring, Summer and Winter pulses - We need make the mean Values for each season #####

### USW9sum was created in the file "Rain and pulses.R" ##########


USW9sum$Season = vector(mode = 'character', length = nrow(USW9sum))
USW9sum$Season[USW9sum$DOY %in% c(1:59,305:366)] = 'Winter'
USW9sum$Season[USW9sum$DOY %in% 60:181] = 'Spring'
USW9sum$Season[USW9sum$DOY %in% 182:304] = 'Summer'

# Winter pulses
Pulse_Win <- USW9sum %>%
  filter(Season == 'Winter')

# Pulses with Max CO2 release (more than 0.6 micromol m-2 s-1)
Pulse_Win_Max <- Pulse_Win %>%
  filter(meanRECO > 0.6)


########## Pulse_model df ###################
Recodf_new$date <- as.Date(Recodf_new$date)
Recodf_new$DOY <- yday(Recodf_new$date)
Recodf_new$year <- substr(Recodf_new$date, 1,4)
Recodf_new$sdReco <- Reco_df$sdReco


# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxWin11 <- Recodf_new %>%
  filter(year == 2013) %>%
  filter(DOY %in% (323:340))

MaxWin21 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (316:333))

MaxWin31 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (343:360))

MaxWin41 <- Recodf_new %>%
  filter(year == 2016) %>%
  filter(DOY %in% (29:46))

# Just not enough days in one year to finish this pulse
MaxWin51 <- Recodf_new %>%
  filter(year == 2016) %>%
  filter(DOY %in% (354:366))

MaxWin61 <- Recodf_new %>%
  filter(year == 2017) %>%
  filter(DOY %in% (348:365))


MaxWin_new <- MaxWin11 %>%
  select(meanRECO, sdReco, DOY, MeanM, Reco_Combined, Comb15)

MaxWin_new$Reco2 <- MaxWin21$meanRECO
MaxWin_new$Reco2sd <- MaxWin21$sdReco
MaxWin_new$MeanM2 <- MaxWin21$MeanM
MaxWin_new$Reco_C2 <- MaxWin21$Reco_Combined
MaxWin_new$Comb16 <- MaxWin21$Comb15

MaxWin_new$Reco3 <- MaxWin31$meanRECO
MaxWin_new$Reco3sd <- MaxWin31$sdReco
MaxWin_new$MeanM3 <- MaxWin31$MeanM
MaxWin_new$Reco_C3 <- MaxWin31$Reco_Combined
MaxWin_new$Comb17 <- MaxWin31$Comb15


MaxWin_new$Reco4 <- MaxWin41$meanRECO
MaxWin_new$Reco4sd <- MaxWin41$sdReco
MaxWin_new$MeanM4 <- MaxWin41$MeanM
MaxWin_new$Reco_C4 <- MaxWin41$Reco_Combined
MaxWin_new$Comb18 <- MaxWin41$Comb15


MaxWin_new$Reco5 <- MaxWin61$meanRECO
MaxWin_new$Reco5sd <- MaxWin61$sdReco
MaxWin_new$MeanM5 <- MaxWin61$MeanM
MaxWin_new$Reco_C5 <- MaxWin61$Reco_Combined
MaxWin_new$Comb19 <- MaxWin61$Comb15


MaxWin_new$meanFlux <- rowMeans(MaxWin_new[,c(1,7,12,17,22)], na.rm=TRUE)
  
MaxWin_new$meanMeanM <- rowMeans(MaxWin_new[,c(4,9,14,19,24)], na.rm=TRUE)  

MaxWin_new$meanReco_Comb <- rowMeans(MaxWin_new[,c(5,10,15,20,25)], na.rm=TRUE)

MaxWin_new$meanReco_15 <- rowMeans(MaxWin_new[,c(6,11,16,21,26)], na.rm=TRUE)

MaxWin_new$Pulse_day = vector(mode = 'character', length = nrow(MaxWin_new))
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 323] = '-3'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 324] = '-2'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 325] = '-1'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 326] = '0'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 327] = '1'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 328] = '2'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 329] = '3'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 330] = '4'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 331] = '5'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 332] = '6'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 333] = '7'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 334] = '8'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 335] = '9'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 336] = '10'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 337] = '11'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 338] = '12'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 339] = '13'
MaxWin_new$Pulse_day[MaxWin_new$DOY %in% 340] = '14'

MaxWin_new$Pulse_day <- as.numeric(as.character(MaxWin_new$Pulse_day))

MaxWin_new$SD_Flux = vector(mode = 'character', length = nrow(MaxWin))
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 323] = 0.18
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 324] = 0.28
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 325] = 0.40
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 326] = 0.03
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 327] = 0.13
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 328] = 0.14
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 329] = 0.04
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 330] = 0.16
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 331] = 0.21
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 332] = 0.19
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 333] = 0.18
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 334] = 0.18
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 335] = 0.18
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 336] = 0.17
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 337] = 0.17
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 338] = 0.18
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 339] = 0.11
MaxWin_new$SD_Flux[MaxWin_new$DOY %in% 340] = 0.11

MaxWin_new$SD_Flux <- as.numeric(as.character(MaxWin_new$SD_Flux))


MaxWin_new %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanFlux),size=2, color = 'blue')+
  geom_point(aes(y=meanMeanM), color = 'red', size=2)+
  geom_point(aes(y=meanReco_Comb), color = 'green', size=2)+
  geom_point(aes(y=meanReco_15), color = 'cyan', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanFlux - (SD_Flux/sqrt(5)), ymax= meanFlux + (SD_Flux/sqrt(5))), width=.2,
                position=position_dodge(.9)) +
  
  xlab('Pulse duration, days')+
  #xlim(-3,14)+
  ggtitle('Mean Winter pulse')+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))


############ Spring pulses 
MaxSpr11 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (99:116))

MaxSpr21 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (174:191))

MaxSpr31 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (175:192))

MaxSpr41 <- Recodf_new %>%
  filter(year == 2016) %>%
  filter(DOY %in% (178:195))

MaxSpr51 <- Recodf_new %>%
  filter(year == 2017) %>%
  filter(DOY %in% (173:190))

MaxSpr61 <- Recodf_new %>%
  filter(year == 2018) %>%
  filter(DOY %in% (164:181))


SprMax_new <- MaxSpr11 %>%
  select(meanRECO, sdReco, DOY, MeanM, Reco_Combined, Comb15)

SprMax_new$Reco2 <- MaxSpr21$meanRECO
SprMax_new$Reco2sd <- MaxSpr21$sdReco
SprMax_new$MeanM2 <- MaxSpr21$MeanM
SprMax_new$Reco_C2 <- MaxSpr21$Reco_Combined
SprMax_new$Comb16 <- MaxSpr21$Comb15

SprMax_new$Reco3 <- MaxSpr31$meanRECO
SprMax_new$Reco3sd <- MaxSpr31$sdReco
SprMax_new$MeanM3 <- MaxSpr31$MeanM
SprMax_new$Reco_C3 <- MaxSpr31$Reco_Combined
SprMax_new$Comb17 <- MaxSpr31$Comb15

SprMax_new$Reco4 <- MaxSpr41$meanRECO
SprMax_new$Reco4sd <- MaxSpr41$sdReco
SprMax_new$MeanM4 <- MaxSpr41$MeanM
SprMax_new$Reco_C4 <- MaxSpr41$Reco_Combined
SprMax_new$Comb18 <- MaxSpr41$Comb15

SprMax_new$Reco5 <- MaxSpr51$meanRECO
SprMax_new$Reco5sd <- MaxSpr51$sdReco
SprMax_new$MeanM5 <- MaxSpr51$MeanM
SprMax_new$Reco_C5 <- MaxSpr51$Reco_Combined
SprMax_new$Comb19 <- MaxSpr51$Comb15

SprMax_new$Reco6 <- MaxSpr61$meanRECO
SprMax_new$Reco6sd <- MaxSpr61$sdReco
SprMax_new$MeanM6 <- MaxSpr61$MeanM
SprMax_new$Reco_C6 <- MaxSpr61$Reco_Combined
SprMax_new$Comb20 <- MaxSpr61$Comb15


SprMax_new$meanFlux <- rowMeans(SprMax_new[,c(1,7,12,17,22,27)], na.rm=TRUE)

SprMax_new$meanMeanM <- rowMeans(SprMax_new[,c(4,9,14,19,24,29)], na.rm=TRUE)  

SprMax_new$meanReco_Comb <- rowMeans(SprMax_new[,c(5,10,15,20,25,30)], na.rm=TRUE)

SprMax_new$meanReco_15 <- rowMeans(SprMax_new[,c(6,11,16,21,26,31)], na.rm=TRUE)

SprMax_new$Pulse_day = vector(mode = 'character', length = nrow(SprMax_new))
SprMax_new$Pulse_day[SprMax_new$DOY %in% 99] = '-3'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 100] = '-2'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 101] = '-1'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 102] = '0'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 103] = '1'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 104] = '2'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 105] = '3'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 106] = '4'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 107] = '5'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 108] = '6'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 109] = '7'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 110] = '8'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 111] = '9'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 112] = '10'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 113] = '11'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 114] = '12'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 115] = '13'
SprMax_new$Pulse_day[SprMax_new$DOY %in% 116] = '14'

SprMax_new$Pulse_day <- as.numeric(as.character(SprMax_new$Pulse_day))

SprMax_new$SD_Flux = vector(mode = 'character', length = nrow(SprMax_new))
SprMax_new$SD_Flux[SprMax_new$DOY %in% 99] = 0.26
SprMax_new$SD_Flux[SprMax_new$DOY %in% 100] = 0.33
SprMax_new$SD_Flux[SprMax_new$DOY %in% 101] = 0.54
SprMax_new$SD_Flux[SprMax_new$DOY %in% 102] = 0.79
SprMax_new$SD_Flux[SprMax_new$DOY %in% 103] = 0.69
SprMax_new$SD_Flux[SprMax_new$DOY %in% 104] = 0.63
SprMax_new$SD_Flux[SprMax_new$DOY %in% 105] = 0.43
SprMax_new$SD_Flux[SprMax_new$DOY %in% 106] = 0.41
SprMax_new$SD_Flux[SprMax_new$DOY %in% 107] = 0.68
SprMax_new$SD_Flux[SprMax_new$DOY %in% 108] = 1.06
SprMax_new$SD_Flux[SprMax_new$DOY %in% 109] = 1.27
SprMax_new$SD_Flux[SprMax_new$DOY %in% 110] = 1.18
SprMax_new$SD_Flux[SprMax_new$DOY %in% 111] = 0.99
SprMax_new$SD_Flux[SprMax_new$DOY %in% 112] = 0.9
SprMax_new$SD_Flux[SprMax_new$DOY %in% 113] = 0.78
SprMax_new$SD_Flux[SprMax_new$DOY %in% 114] = 0.67
SprMax_new$SD_Flux[SprMax_new$DOY %in% 115] = 0.62
SprMax_new$SD_Flux[SprMax_new$DOY %in% 116] = 0.75

SprMax_new$SD_Flux <- as.numeric(as.character(SprMax_new$SD_Flux))


SprMax_new %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanFlux),size=2, color = 'blue')+
  geom_point(aes(y=meanMeanM), color = 'red', size=2)+
  geom_point(aes(y=meanReco_Comb), color = 'green', size=2)+
  geom_point(aes(y=meanReco_15), color = 'cyan', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanFlux - (SD_Flux/sqrt(6)), ymax= meanFlux + (SD_Flux/sqrt(6))), width=.2,
                position=position_dodge(.9)) +
  
  xlab('Pulse duration, days')+
  #xlim(-3,14)+
  ggtitle('Mean Spring pulse')+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))




# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxSum11 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (181:198))

MaxSum21 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (183:200))

MaxSum31 <- Recodf_new %>%
  filter(year == 2015) %>%
  filter(DOY %in% (209:226))

MaxSum41 <- Recodf_new %>%
  filter(year == 2016) %>%
  filter(DOY %in% (196:213))

MaxSum51 <- Recodf_new %>%
  filter(year == 2016) %>%
  filter(DOY %in% (197:214))

MaxSum61 <- Recodf_new %>%
  filter(year == 2018) %>%
  filter(DOY %in% (216:233))

MaxSum71 <- Recodf_new %>%
  filter(year == 2019) %>%
  filter(DOY %in% (237:254))


MaxSum_new <- MaxSum11 %>%
  select(meanRECO, sdReco, meanRECO, sdReco, DOY, MeanM, Reco_Combined, Comb15)

MaxSum_new$Reco2 <- MaxSum21$meanRECO
MaxSum_new$Reco2sd <- MaxSum21$sdReco
MaxSum_new$MeanM2 <- MaxSum21$MeanM
MaxSum_new$Reco_C2 <- MaxSum21$Reco_Combined
MaxSum_new$Comb16 <- MaxSum21$Comb15

MaxSum_new$Reco3 <- MaxSum31$meanRECO
MaxSum_new$Reco3sd <- MaxSum31$sdReco
MaxSum_new$MeanM3 <- MaxSum31$MeanM
MaxSum_new$Reco_C3 <- MaxSum31$Reco_Combined
MaxSum_new$Comb17 <- MaxSum31$Comb15

MaxSum_new$Reco4 <- MaxSum41$meanRECO
MaxSum_new$Reco4sd <- MaxSum41$sdReco
MaxSum_new$MeanM4 <- MaxSum41$MeanM
MaxSum_new$Reco_C4 <- MaxSum41$Reco_Combined
MaxSum_new$Comb18 <- MaxSum41$Comb15

MaxSum_new$Reco5 <- MaxSum51$meanRECO
MaxSum_new$Reco5sd <- MaxSum51$sdReco
MaxSum_new$MeanM5 <- MaxSum51$MeanM
MaxSum_new$Reco_C5 <- MaxSum51$Reco_Combined
MaxSum_new$Comb19 <- MaxSum51$Comb15

MaxSum_new$Reco6 <- MaxSum61$meanRECO
MaxSum_new$Reco6sd <- MaxSum61$sdReco
MaxSum_new$MeanM6 <- MaxSum61$MeanM
MaxSum_new$Reco_C6 <- MaxSum61$Reco_Combined
MaxSum_new$Comb20 <- MaxSum61$Comb15

MaxSum_new$Reco7 <- MaxSum71$meanRECO
MaxSum_new$Reco7sd <- MaxSum71$sdReco
MaxSum_new$MeanM7 <- MaxSum71$MeanM
MaxSum_new$Reco_C7 <- MaxSum71$Reco_Combined
MaxSum_new$Comb21 <- MaxSum71$Comb15

MaxSum_new$meanFlux <- rowMeans(MaxSum_new[,c(1,7,12,17,22,27,32)], na.rm=TRUE)

MaxSum_new$meanMeanM <- rowMeans(MaxSum_new[,c(4,9,14,19,24,29,34)], na.rm=TRUE)  

MaxSum_new$meanReco_Comb <- rowMeans(MaxSum_new[,c(5,10,15,20,25,30,35)], na.rm=TRUE)

MaxSum_new$meanReco_15 <- rowMeans(MaxSum_new[,c(6,11,16,21,26,31,36)], na.rm=TRUE)

MaxSum_new$Pulse_day = vector(mode = 'character', length = nrow(MaxSum_new))
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 181] = '-3'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 182] = '-2'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 183] = '-1'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 184] = '0'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 185] = '1'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 186] = '2'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 187] = '3'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 188] = '4'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 189] = '5'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 190] = '6'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 191] = '7'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 192] = '8'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 193] = '9'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 194] = '10'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 195] = '11'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 196] = '12'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 197] = '13'
MaxSum_new$Pulse_day[MaxSum_new$DOY %in% 198] = '14'

MaxSum_new$Pulse_day <- as.numeric(as.character(MaxSum_new$Pulse_day))

MaxSum_new$SD_Flux = vector(mode = 'character', length = nrow(MaxSum_new))
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 181] = 0.36
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 182] = 0.52
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 183] = 0.39
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 184] = 0.59
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 185] = 0.39
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 186] = 0.35
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 187] = 0.46
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 188] = 0.41
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 189] = 0.5
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 190] = 0.34
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 191] = 0.46
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 192] = 0.48
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 193] = 0.52
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 194] = 0.34
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 195] = 0.37
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 196] = 0.39
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 197] = 0.26
MaxSum_new$SD_Flux[MaxSum_new$DOY %in% 198] = 0.34

MaxSum_new$SD_Flux <- as.numeric(as.character(MaxSum_new$SD_Flux))


MaxSum_new %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanFlux),size=2, color = 'blue')+
  geom_point(aes(y=meanMeanM), color = 'red', size=2)+
  geom_point(aes(y=meanReco_Comb), color = 'green', size=2)+
  geom_point(aes(y=meanReco_15), color = 'cyan', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanFlux - (SD_Flux/sqrt(7)), ymax= meanFlux + (SD_Flux/sqrt(7))), width=.2,
                position=position_dodge(.9)
                ) +
  
  xlab('Pulse duration, days')+
  #xlim(-3,14)+
  ggtitle('Mean Summer pulse')+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))












