# 05-05-2022
# 21:56
# Anastasia Makhnykina

library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(tidyr)
library(ggplot2)
library(Hmisc)

# Making the Interval Graph

getwd()
freqrainman <- read.csv("data/Complete Irrigation.csv",
                        sep=";",header=TRUE, na.strings = "NaN")

freqrainman <- read.csv("data/Complete Irrigation.csv",
                        sep=";",header=TRUE, na.strings = ".")

flux_rainman <- read.csv("data/AllColumns_FirstCleanMethod.csv",
                        sep=",",header=TRUE, na.strings = "NaN")

plot(freqrainman$S1.mm.)
typeof(freqrainman$S1.mm.)

#create DOY from date field
freqrainman$DOY <- as.numeric(paste(yday(freqrainman$Date)))


###############################################################
# Create histograms of rainfall, Rsoil and intervals
###############################################################

freqrainman %>%
  na.omit() %>%
  ggplot(aes(x=S1.mm.))+
  geom_histogram()+
  theme_classic()+
  xlab('Rain (mm)')

hist(freqrainman$S2.mm.)

#Try to replicate this
#note 
# `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#https://github.com/davidjpmoore/Fsoil_aridlands/blob/6621db9262a9af9e19c883ac89d358e9d5ce6cff/Figures/18%20Rain%20events%205mm.jpeg

#to calculate the time since last rain event
freqrainman$Previous_DOY_S1 <- lag(freqrainman$DOY)
#note there are N/A values in S2 and S4 - we could replace them with zeros
#freqrainman$DOY <- as.numeric(as.character(freqrainman$DOY))
# freqrainman$S2.mm. <- as.numeric(as.character(freqrainman$S2.mm.))
# freqrainman$S3.mm. <- as.numeric(as.character(freqrainman$S3.mm.))
# freqrainman$S4.mm. <- as.numeric(as.character(freqrainman$S4.mm.))
#freqrainman$Previous_DOY_S1 <- as.numeric(as.character(freqrainman$Previous_DOY_S1))

freqrainman$Timeinbetween_S1 <- freqrainman$DOY - freqrainman$Previous_DOY_S1
#need to remove or repair the year transition (i.e. add 365 or 366)





# create multiple histogram
hist(freqrainman$S1.mm., col='red')
hist(freqrainman$S2.mm., col='green', add=TRUE)
hist(freqrainman$S3.mm., col='blue', add=TRUE)
hist(freqrainman$S4.mm., col='yellow', add=TRUE)
#need to create a histogram for all the raindata

freqrainman %>%
  na.omit() %>%
  filter(Timeinbetween_S1>0) %>%
  ggplot(aes(x=Timeinbetween_S1))+
  geom_bar()+
  theme_classic()



### stopped Friday 6th of May


###############################################################
# Create plots of pulse sizes in T and Moisture space
###############################################################

###############################################################
# get temp and moisture data from Rainman data folder
###############################################################
# https://drive.google.com/file/d/1Rl4D9sPZDCLetVaIDdVEju295XWOQ20x/view?usp=sharing
#go to Rainman Raw Data to 


# work out how to stop syncing data
# explore possibility of reading data directly from google drive

# Fluxes part
#flux_rainman$DOY <- paste(yday(flux_rainman$ï..Date))
flux_rainman$DOY <- paste(yday(flux_rainman$Date))

flux_rainman %>%
  group_by(S) %>%
  group_by(DOY) %>%
  filter(House == 1) %>%
  summarise(meanRs=mean(Flux)) %>%
  ggplot(aes(x=DOY,y=meanRs))+
  geom_point()+
 # scale_x_continuous(name="Day of Year", limits=c(0, 366)) +
 #  scale_y_continuous(name="Mean Respiration", limits=c(0, 3))+
  theme_classic()
#add some formatting 



#Goal is to replicate the graphs we made for Kendall 
# https://github.com/davidjpmoore/Fsoil_aridlands/blob/6621db9262a9af9e19c883ac89d358e9d5ce6cff/Figures/18%20Pulses%20sum%20rain.jpeg
#and
# 

flux_rainman_sum <- flux_rainman %>%
  filter(House == 1) %>%
  group_by(S) %>%
  summarise(meanRs=mean(DOY), Sum_Treatment=S)


flux_rainman_sum <- flux_rainman %>%
  group_by(House) %>%
  summarise(meanRs=mean(Flux), Sum_Treatment=S)







