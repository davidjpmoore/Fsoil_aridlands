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
#library(Hmisc)
#package does not install

# The ID of a file can be found by navigating to the 
# Google Drive web. Right click on the file you need, and 
# select get sharable link . 
# The File ID is the last part of that link, after id= .

#https://drive.google.com/drive/folders/1An5QfaLByd2CadyI5Bg_T8-mz89rYHlf?usp=sharing


############################################################################
# Read in files from data store
############################################################################

############################################################################
#### freq dataset reports the rainfall recieved by each irrigation treatment
############################################################################
freqrainman <- read.csv("data/Complete Irrigation.csv",
                        sep=";",header=TRUE, na.strings = ".")
freqrainman$Date10 <- as.Date(substr(freqrainman$Date, 1,10))
#formating the 10 digit date as a DATE
#create DOY from date field
freqrainman$DOY <- as.numeric(paste(yday(freqrainman$Date)))
#need to document this file (where does it come from?)

#using pivot_longer to change columns to rows
freqrainman_slim <-
  pivot_longer(data= freqrainman,
               cols = starts_with("S"),
               names_to = "Summer_mm",
               values_to = "Precip"
  )
#add Summer to be consistent with other datasets
freqrainman_slim$Summer <- as.character(substr(freqrainman_slim$Summer_mm, 1,2))
freqrainman_slim$Precip[is.na(freqrainman_slim$Precip)] <- 0
freqrainman_slim <- subset(freqrainman_slim, select=-c(Summer_mm, Date, DOY))


Precip_daily =freqrainman_slim %>%
  group_by(Date10, Summer) %>% 
  summarize(
    Precip_daily = sum(Precip,na.rm=TRUE) )

plot(Precip_daily$Precip_daily)

plot(freqrainman_slim$Date10,freqrainman_slim$Precip)
plot(freqrainman$Date10,freqrainman$S4.mm.)
plot(freqrainman$S1.mm.)
plot(freqrainman$S2.mm.)
plot(freqrainman$S3.mm.)
############################################################################
#
# Soil Efflux data sent from Jake
#
############################################################################

flux_rainman <- read.csv("data/AllColumns_FirstCleanMethod.csv",
                         sep=",",header=TRUE, na.strings = "NaN")
flux_rainman$Date10 <- as.Date(substr(flux_rainman$Date, 1,10))
#formating the 10 digit date as a DATE
flux_rainman$DOY <- as.numeric(paste(yday(flux_rainman$Date10)))
#need to document this file (where does it come from? email from Jake?)

flux_RM <-  flux_rainman %>% 
  mutate(Plot = recode(Plot, '1' = '01', '2' = '02', 
                       '3' = '03', '4' = '04', '5' = '05','6' = '06', 
                       '7' = '07', '8' = '08', '9' = '09', 
                       '10' = '10', '11' = '11', '12' = '12'))  %>%
  mutate(House = recode(House, '1' = '01', '2' = '02', 
                        '3' = '03', '4' = '04', '5' = '05')) %>%
  rename(Summer=S, Winter=W)

flux_RM$Sum_Wint_TRT = paste(flux_RM$Winter, flux_RM$Summer, sep="_")
flux_RM$Plot = as.character(flux_RM$Plot)
flux_RM$House = as.character(flux_RM$House)
flux_RM$House_plot = paste(flux_RM$House, flux_RM$Plot, sep="_")
#columns above created for a join check - join check passed
#removing columns to avoid duplications
flux_RM <- subset(flux_RM, select = -c(Winter, Summer, Sum_Wint_TRT))

########### from the treatement definition
# https://docs.google.com/spreadsheets/d/1eL_CKVklv2upHVd3Hck9stpddoQscd22yFScTaqrdxM/edit#gid=853864262
# House	Plot	Winter	Summer
# 1	1	W3	S4
# 1	2	W2	S2
# 1	3	W3	S1
# 1	4	W1	S1
# 1	5	W1	S3
# 1	6	W3	S2
# 1	7	W2	S1
# 1	8	W1	S4
# 1	9	W1	S2
# 1	10	W2	S3
# 1	11	W3	S3
# 1	12	W2	S4

#For example House 1, Plot 2 should be	W2	S2
#flux_RM_check = subset(flux_RM, select = c(House, Plot, Winter, Summer))

###############################################################
# read treatment codes from RAINMAN DATA STORE
# https://docs.google.com/spreadsheets/d/1eL_CKVklv2upHVd3Hck9stpddoQscd22yFScTaqrdxM/edit#gid=853864262
# This file was downloaded to a CSV file Path= "~/data/TreatmentCODED.csv"
###############################################################

TRT_RM <- read.csv("data/TreatmentCODED.csv",
                   sep=",",header=TRUE, na.strings = "NA")
TRT_RM <-  TRT_RM %>% 
  mutate(Plot = recode(Plot, '1' = '01', '2' = '02', 
                       '3' = '03', '4' = '04', '5' = '05','6' = '06', 
                       '7' = '07', '8' = '08', '9' = '09', 
                       '10' = '10', '11' = '11', '12' = '12'))  %>%
  mutate(House = recode(House, '1' = '01', '2' = '02', 
                        '3' = '03', '4' = '04', '5' = '05')) 

TRT_RM$Sum_Wint_TRT = paste(TRT_RM$Winter, TRT_RM$Summer, sep="_")
TRT_RM$Plot = as.character(TRT_RM$Plot)
TRT_RM$House = as.character(TRT_RM$House)
TRT_RM$House_plot = paste(TRT_RM$House, TRT_RM$Plot, sep="_")
#slim down TRT 
TRT_RM_mge=subset(TRT_RM, select = c(House_plot, Winter, Summer, Sum_Wint_TRT))

###############################################################


#Join with Treatment information for check
#Note there are more treatment combinations than VWC probes
flux_RM_jn =  full_join( TRT_RM_mge,flux_RM,
                         by = c("House_plot" = "House_plot"))
flux_RM = subset(flux_RM_jn, 
                 select = c(Date10 ,DOY, House, Plot, Winter, Summer, 
                     Sum_Wint_TRT, House_plot,  Tchamb., Flux, R2))


 flux_RM %>%
#  mutate(Daily = day(Date10)) %>%
  group_by(Date10, House_plot, na.rm=TRUE) 
 
 plot( flux_RM$Precip, na.rm=TRUE)
 
 flux_RM_daily =flux_RM %>%
   group_by(Date10, House_plot, Summer) %>% 
  summarize(
    Fsoil_daily = sum(Flux, na.rm=TRUE),
    Tcham_daily = mean(Tchamb., na.rm=TRUE) )

 
# PlotFlux by Treatment in RAINMAN  
 Flux_byTreat <- ggplot(flux_RM_daily, aes(x=Date10, y=Fsoil_daily, group=Summer, color=Summer)) +
   #Note that I have stored the Date10 variable as.Date 
   #this allows me to use date functions like this
   scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
   geom_line() 
 
 
 # to display this plot type VWCplotRaw and add any theme you'd like to it
 Flux_byTreat + theme_classic() 

#  
#  ########################################################### 
# # READ DAILY VWC data unless already created
# # file="data/VWC_daily.csv"
#  ###########################################################
 VWC_slim_RM_daily <- read.csv("data/VWC_daily.csv",
                     sep=",",header=TRUE, na.strings = "NA")
 VWC_slim_RM_daily$Date10 = as.Date(VWC_slim_RM_daily$Date10)
 #  ###########################################################
 
 Precip_daily
 flux_RM =  full_join(flux_RM,freqrainman_slim,  
                      by = c("Date10" = "Date10", "Summer" = "Summer"))
 #  ###########################################################
 #  Merge daily rainman flux with VWC
 #  ###########################################################
#VWC_slim
flux_RM_VWC =full_join( flux_RM_daily,VWC_slim_RM_daily, by = c("House_plot" = "House_plot", "Date10" = "Date10", "Summer"="Summer"))

 flux_RM_VWC_precip =full_join( flux_RM_VWC,Precip_daily, by = c("Date10" = "Date10", "Summer"="Summer"))
 
 
 
 ################
# quick plots
################

# PlotFlux by Treatment in RAINMAN  
Flux_byTreat <- ggplot(flux_RM_VWC, aes(x=Date10, y=Fsoil_daily, group=Summer, color=Summer)) +
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  geom_line() 


# to display this plot type the name of the object and add any theme you'd like to it
Flux_byTreat + theme_classic() 

# plot VWC by Treatment in RAINMAN  
VWC_byTreat <- ggplot(flux_RM_VWC, aes(x=Date10, y=VWC_daily, group=Summer, color=Summer)) +
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  geom_line() 


# to display this plot type the name of the object and add any theme you'd like to it
VWC_byTreat + theme_classic() 


# plot PRECIP by Treatment in RAINMAN  
Precip_byTreat <- ggplot(flux_RM_VWC_precip, aes(x=Date10, y=Precip_daily, group=Summer, color=Summer)) +
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  geom_line() 
#######################
# this is wrong! the precip is not summed up correctly 
#######################

# to display this plot type the name of the object and add any theme you'd like to it
Precip_byTreat + theme_classic() 


# plot VWC by Treatment in RAINMAN  
Flux_by_VWC <- ggplot(flux_RM_VWC, aes(x=VWC_daily, y=Fsoil_daily, group=Summer, color=Summer)) +
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  #scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  geom_line() 

# to display this plot type the name of the object and add any theme you'd like to it
Flux_by_VWC + theme_classic() 



# Making the Interval Graph 

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




# 
# # create multiple histogram
# hist(freqrainman$S1.mm., col='red')
# hist(freqrainman$S2.mm., col='green', add=TRUE)
# hist(freqrainman$S3.mm., col='blue', add=TRUE)
# hist(freqrainman$S4.mm., col='yellow', add=TRUE)
# #need to create a histogram for all the raindata
# 
# freqrainman %>%
#   na.omit() %>%
#   filter(Timeinbetween_S1>0) %>%
#   ggplot(aes(x=Timeinbetween_S1))+
#   geom_bar()+
#   theme_classic()



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







