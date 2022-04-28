#4/27/2022
#Anastasia Makhnykina

#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)



#open cvs
data_Chambers=read.csv("data/kendall2017.csv", header=TRUE, na.strings = "NaN", sep = ";")

# Calculate DOY
data_Chambers$date <- paste(data_Chambers$year, data_Chambers$month, data_Chambers$day, sep="-")
data_Chambers$time <- paste(data_Chambers$hour,data_Chambers$min, sep=":")
data_Chambers$DOY <- paste(yday(data_Chambers$date))

data_Chambers$date <- as_date(data_Chambers$date)


#Calculate the mean Rsoil for each DOY
dataRsoil <- data_Chambers %>%
  group_by(as.numeric(DOY)) %>%
  dplyr::summarize(meanSR1=mean(Fs_1, na.rm=TRUE), 
            meanSR2=mean(Fs_2, na.rm=TRUE), 
            meanSR3=mean(Fs_3, na.rm=TRUE),
            meanSR4=mean(Fs_4, na.rm=TRUE),
            meanST1=mean(Ts_1, na.rm=TRUE), 
            meanST2=mean(TFs_2, na.rm=TRUE), 
            meanST3=mean(TFs_3, na.rm=TRUE), 
            meanST4=mean(Ts_4, na.rm=TRUE), 
            meanSM1=mean(VWC_1, na.rm=TRUE), 
            meanSM2=mean(VWC_2, na.rm=TRUE),
            meanSM3=mean(VWC_3, na.rm=TRUE), 
            meanSM4=mean(VWC_4, na.rm=TRUE))




















