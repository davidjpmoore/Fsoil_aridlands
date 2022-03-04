#analysis basics based on code from SRGarrity
#https://github.com/SRGarrity/Ameriflux-Analysis/blob/master/analysis_code.R
library(tidyverse)
library(ggplot2)
library(fpp)
library(readr)
library(lubridate)
library(anytime)
library(imputeTS)
library(shiny)
library(openair)

# #read Kendall Grassland files
# US_Wkg = read.csv("data/AMF_US-Wkg_BASE_HH_18-5.csv" ,  skip =2,  
#                   comment.char = "",check.names = FALSE, quote="",
#                   na.strings=c("NA","NaN", " ", "-9999") )
# 
# RsoilWkg<- read.csv("data/kendall2017.csv",  skip =0,  
#                     comment.char = "",check.names = FALSE, quote="",
#                     na.strings=c("NA","NaN", " ") )

str(US_Wkg)

# convert timestamp to something R-friendly:
US_Wkg$timestamp <- parse_date_time2(as.character(US_Wkg$TIMESTAMP_START), orders=c("Y!-m!-d! H!:M!"), tz="America/Detroit")

# start by looking at data from 2010-2014:
US_Wkg <- US_Wkg[US_Wkg$TIMESTAMP_START>200812312300,]
dim(US_Wkg)

US_Wkg %>% ggplot(aes(x=timestamp, y=FC)) +
  geom_line() +
  labs(x="", y=expression(paste("Net Ecosystem Exchange (", mu,"mol m"^"-2", "s"^"-1",")")))

# decompose time series:
dNEE <- ts(US_Wkg$FC, start = US_Wkg$timestamp[1], frequency=)
dNEE <- na.seadec(dNEE)
plot(stl(dNEE, s.window = "periodic", robust=TRUE, s.degree = 1))

#calculate daily, weekly and monthly averages
library(openair)
US_Wkg$date <- US_Wkg$timestamp # add "date" column so that the "timeAverage" function will work
US_Wkgdaily <- timeAverage(US_Wkg, avg.time="day")
US_Wkgweekly <- timeAverage(US_Wkg, avg.time="week")
US_Wkgmonthly <- timeAverage(US_Wkg, avg.time="month")
library(cli)
US_WkgdailySUM <- timeAverage(US_Wkg, avg.time="day", statistic = "sum")
US_WkgweeklySUM <- timeAverage(US_Wkg, avg.time="week", statistic = "sum")
US_WkgmonthlySUM <- timeAverage(US_Wkg, avg.time="month", statistic = "sum")

#  The statistic to apply when aggregating the data; default is the mean.
# Can be one of “mean”, “max”, “min”, “median”, “frequency”, “sum”, “sd”,
# “percentile”. Note that “sd” is the standard deviation, “frequency” is 
# the number (frequency) of valid records in the period and “data.cap” is 
# the percentage data capture. “percentile” is the percentile level (%) 
# between 0-100, which can be set using the “percentile” option. Not used 
# if avg.time = "default".

plot(US_Wkgdaily$FC)
plot(US_WkgdailySUM$date, US_WkgdailySUM$FC)

