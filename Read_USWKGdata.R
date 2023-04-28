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
USWkg12_18 <- read.csv("data/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv", header=TRUE, na.strings="NaN", skip=0)
USWkg18_19 <- read.csv("data/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv", header=TRUE, na.strings="NaN", skip=0)
USWkg19_20 <- read.csv("data/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv", header=TRUE, na.strings="NaN", skip=0)
  

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
 




