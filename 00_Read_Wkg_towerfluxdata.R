################################################################################
# Author:David Moore  
# Date:Jan 16th 2022
# Purpose:Grab Wkg data from Russ Scott via email June 10
################################################################################
# Ok, here is the 2017-2020 flux tower data that I submit to Ameriflux but 
# I've added columns for gap-filled NEE, ER, and GEP (all units of micromol 
# CO2/m2/s) to them.  There is also a header spreadsheet which further 
# explains the data.   Note after 2018, there are some extra columns added 
# to include new soil moisture profile data..... Russ

#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)
library(PerformanceAnalytics)
library(xtable)
#library(Hmisc)
library(ggpubr)
library(ggplot2)
library(colorRamps)


#Load headers for data
WkgHeaders=read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/Wkg_Ameriflux_headers.csv", header=TRUE, na.strings = "NaN")

#load flux data 2016-2017
WkgFluxes_16_17=read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/AddedPartionedCflux_US-Wkg_HH_201612312330_201712312330.csv", header=TRUE, na.strings = "NaN")
#load flux data 2017-2018
WkgFluxes_17_18=read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/AddedPartionedCflux_US-Wkg_HH_201712312330_201812312330.csv", header=TRUE, na.strings = "NaN")
#load flux data 2018-2019
WkgFluxes_18_19=read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv", header=TRUE, na.strings = "NaN")
#load flux data 2019-2020
WkgFluxes_19_20=read.csv("data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv", header=TRUE, na.strings = "NaN")


#load SoiR data from Wkg
WkgSoilR_17_20 =read.csv("data/KN_soil_resp17_20_longHead.csv", header=TRUE, na.strings = "NaN")


# US-WKG	Walnut Gulch Grassland																					
# Ports 1 - 4	no treatment																					
# Ports 5 -7	sites trenched and weeded to prevent root respiration																					
# Soil Resp.	VWC	Soil Temp.
# um co2/m2/s		deg C

#remove unnecessary columns


##