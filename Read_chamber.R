# 16-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

###### Read soil respiration data from chamber measurements ###
###### for all existing seasons ###############################


#Read soil chamber data provided by Russ Scott
CH_USWkg17_20 <- read.csv("data/KN_soil_resp17_20_longHead.csv", 
                          header=TRUE, na.strings="NaN", skip=0)

#check date format
CH_USWkg17_20$date = as.Date(paste(CH_USWkg17_20$Year+2000, 
                                   CH_USWkg17_20$DOY, sep = "-"), format = "%Y-%j")


write.csv(CH_USWkg17_20, file="data/Chamber data.csv")








