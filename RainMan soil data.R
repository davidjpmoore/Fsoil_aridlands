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
freqrainman <- read.csv("C:/Users/sunlife1408/Documents/RainMan2022/Fsoil_aridlands/data/Complete Irrigation.csv",
                        sep=";",header=TRUE, na.strings = "NaN")

flux_rainman <- read.csv("C:/Users/sunlife1408/Documents/RainMan2022/Fsoil_aridlands/data/AllColumns_FirstCleanMethod.csv",
                        sep=",",header=TRUE, na.strings = "NaN")

plot(freqrainman$S1.mm.)
typeof(freqrainman$S1.mm.)

freqrainman$DOY <- paste(yday(freqrainman$Date))

freqrainman %>%
  na.omit() %>%
  ggplot(aes(x=S1.mm.))+
  geom_histogram()+
  theme_classic()+
  xlab('Rain (mm)')


freqrainman$Previous_DOY_S1 <- lag(freqrainman$DOY)

freqrainman$DOY <- as.numeric(as.character(freqrainman$DOY))
freqrainman$S2.mm. <- as.numeric(as.character(freqrainman$S2.mm.))
freqrainman$S3.mm. <- as.numeric(as.character(freqrainman$S3.mm.))
freqrainman$S4.mm. <- as.numeric(as.character(freqrainman$S4.mm.))
freqrainman$Previous_DOY_S1 <- as.numeric(as.character(freqrainman$Previous_DOY_S1))

freqrainman$Timeinbetween_S1 <- freqrainman$DOY - freqrainman$Previous_DOY_S1

freqrainman$Timeinbetween_S1 <- as.numeric(as.double(freqrainman$Timeinbetween_S1))

hist(freqrainman$S2.mm.)

# create multiple histogram
hist(freqrainman$S1.mm., col='red')
hist(freqrainman$S2.mm., col='green', add=TRUE)
hist(freqrainman$S3.mm., col='blue', add=TRUE)
hist(freqrainman$S4.mm., col='yellow', add=TRUE)


freqrainman %>%
  na.omit() %>%
  ggplot(aes(x=Timeinbetween_S1))+
  geom_bar()+
  theme_classic()



# Fluxes part
flux_rainman$DOY <- paste(yday(flux_rainman$ï..Date))

flux_rainman %>%
  group_by(S) %>%
  group_by(DOY) %>%
  filter(House == 1) %>%
  summarise(meanRs=mean(Flux)) %>%
  ggplot(aes(x=DOY,y=meanRs))+
  geom_point()+
  theme_classic()

flux_rainman_sum <- datarain %>%
  group_by(DOY) %>%
  group_by(S) %>%
  summarise(meanRs=mean(Flux))










