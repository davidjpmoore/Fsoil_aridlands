###############################################################
#Author: Dave Moore
#Date: 05/07/2022
#Purpose: read in the VWC data from rainman
# extract plot, probe and house data from VWC column headers
# graph data by plot
###############################################################
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(tidyr)
library(ggplot2)

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


###############################################################
# READ VWC data from RAINMAN
#need to document this file (where does it come from?)
# This was emailed to Dave Moore by Anastasia but 
# Dave cannot find the location of the file in the 
# RAINMAN data store
###############################################################
VWC_rainman <- read.csv("data/Halfhourly_VWC_Weather_112721.csv",
                        sep=",",header=TRUE, na.strings = "NA")


###############################################################
#add date format and clean up#
VWC_rainman$Date10 <- as.Date(substr(VWC_rainman$TIMESTAMP, 1,10))
#formating the 10 digit date as a DATE
VWC_rainman$DOY <- as.numeric(paste(yday(VWC_rainman$Date10)))

#using pivot_longer to change columns to rows
VWC_slim <-
  pivot_longer(data= VWC_rainman,
               cols = starts_with("VWC_"),
               names_to = "PLOT_PROBE_HOUSE",
               values_to = "VWC"
  )

#column names are stored as VWC_PX_X_HX or VWC_PXX_X_HX
#to separate these I used "_" as a separater
#https://stackoverflow.com/questions/7069076/split-column-at-delimiter-in-data-frame
#unfortunately this results in plots either 2 or 3 characters in length
VWC_slim <-
  separate(data = VWC_slim, 
           col = PLOT_PROBE_HOUSE, 
           into = c("Var", "Plot", "Probe", "House"), 
           sep = "_")
#solving this issue by recoding Plot to a Char with length 2
VWC_slim <-  VWC_slim %>% mutate(Plot = recode(Plot, 'P1' = '01', 'P2' = '02', 
                                               'P3' = '03', 'P4' = '04', 'P5' = '05','P6' = '06', 
                                               'P7' = '07', 'P8' = '08', 'P9' = '09', 
                                               'P10' = '10', 'P11' = '11', 'P12' = '12')) %>% 
                          mutate(House = recode(House, 'H1' = '01', 'H2' = '02', 
                                               'H3' = '03', 'H4' = '04', 'H5' = '05')) %>% 
                          mutate(House_plot = paste(House, Plot, sep="_"))

#now there are two many columns and they are in the wrong order so I'm selecting
#only the columns that I want and reordering them so that they make sense to me
#https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
VWC_slim = subset(VWC_slim, select = c(TIMESTAMP, Date10 ,DOY, House, Plot, House_plot, Probe, T_inside, RH_inside, RH_outside,VWC))
TRT_RM_mge=subset(TRT_RM, select = c(House_plot, Winter, Summer, Sum_Wint_TRT))

#Join with Treatment information.
#Note there are more treatment combinations than VWC probes
VWC_slim =  full_join( TRT_RM_mge,VWC_slim, by = c("House_plot" = "House_plot"))
   
#############################################
#####     Calculate daily averages      #####
#############################################
VWC_slim_RM_daily =VWC_slim %>%
  group_by(Date10, House_plot, Summer) %>% 
  summarize(
    VWC_daily = mean(VWC, na.rm=TRUE),
    T_inside_daily = mean(T_inside, na.rm=TRUE))
#Writing out daily file
#write_csv(VWC_slim_RM_daily, file="data/VWC_daily.csv")

############################################################
# Plotting all the soil moisture RAW
############################################################
#here I am assiging the ggplot to an object "VWCplot" this will 
#allow you to save the plot or display it later
VWCplotRaw <- ggplot(VWC_slim, aes(x=Date10, y=VWC, group=Sum_Wint_TRT, color=Sum_Wint_TRT)) +
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  geom_line() 


# to display this plot type VWCplotRaw and add any theme you'd like to it
VWCplotRaw + theme_classic() 