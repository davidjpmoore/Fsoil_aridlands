# 16-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)


##### Read data from eddy tower for all seasons #######################

setwd("C:\\Users\\user\\Documents\\Fsoil_aridlands")


# First file upload from the folder on your computer because it's too big

USWkg12_18 <- read.csv("C:/Users/user/Documents/Data Fsoil/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv", header=TRUE, na.strings="NaN", skip=0)

# Other files from the Data folder in GitHub

USWkg18_19 <- read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv", header=TRUE, na.strings="NaN", skip=0)
USWkg19_20 <- read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv", header=TRUE, na.strings="NaN", skip=0)

# The next step is combining them together using the common column

USWkg12_18$T_CANOPY_2_1_1 <- as.numeric(USWkg12_18$T_CANOPY_2_1_1)
USWkg18_19$T_CANOPY_2_1_1 <- as.numeric(USWkg18_19$T_CANOPY_2_1_1)
USWkg19_20$T_CANOPY_2_1_1 <- as.numeric(USWkg19_20$T_CANOPY_2_1_1)
USWkg12_20 <- bind_rows(USWkg12_18, USWkg18_19, USWkg19_20)


#Replace -9999 with NA for all data
USWkg12_20[USWkg12_20 == -9999] <- NA

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

# Create new columns to characterize precipitation conditions
USWkg12_20$RainEvent_0 <- as.numeric(USWkg12_20$P>0)
USWkg12_20$RainEvent_5 <- as.numeric(USWkg12_20$P>5)

# Rename some columns with long names

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
    sdSWC5 = sd(SWC5, na.rm=TRUE),
    meanSWC15 = mean(SWC15, na.rm = TRUE),
    sdSWC15 = sd(SWC15, na.rm=TRUE),
    meanSWC30 = mean(SWC30, na.rm = TRUE),
    meanST5 = mean(ST5, na.rm = TRUE),
    meanST15 = mean(ST15, na.rm = TRUE),
    meanST30 = mean(ST30, na.rm = TRUE),
    meanNEE = mean(NEE, na.rm = TRUE),
    meanGPP = mean(GPP, na.rm = TRUE),
    meanRECO = mean(RECO, na.rm = TRUE),
    sdReco = sd(RECO, na.rm = TRUE)
  )

write_csv(USWkg12_20_summary, "data/USWkg12_20_summary.csv")

