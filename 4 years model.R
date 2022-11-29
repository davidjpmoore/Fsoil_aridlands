# 29-11-2022
# Anastasia Makhnykina

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
library(Hmisc)
library(ggpubr)
library(ggplot2)
library(colorRamps)
library(reshape2)
library(zoo)
library(deSolve)
library(stats)
library(drc)

years_sum1 <- rbind(summary2017_new,  summary2018_new, summary2019_new, summary2020_new)

years_sum1[is.na(years_sum1)] = 0

years_sum_Pulse0 <- years_sum1 %>%
  filter(Pulse_DOY == 0)

years_sum_Pulse1 <- years_sum1 %>%
  filter(Pulse_DOY > 0)

########## drc package + drm function

Fref = 0.75
SWCopt = 0.25


model7 <- drm(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = years_sum1,
              fct = LL.4 (names = c("meanRECO","meanSWC30", "meanST30", "meanGPP")),
              start = list(SWCopt = 0.25, Fref=0.75)
)


model77 <- drm(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = years_sum_Pulse0,fct = LL.2 (names = c("SWCopt","Fref")),
              start = list(SWCopt = 0.25, Fref=0.75)
)

################# For soil respiration

SRdata =read.csv("data/dataRsoil.csv", sep = ",",
                     header=TRUE, na.strings = "NaN")
SRdata111 =read.csv("data/kendall2017.csv", sep = ";",
                 header=TRUE, na.strings = "NaN")

Fref = 0.75
SWCopt = 0.25

SRdata$meanSM1 <- as.numeric(SRdata$meanSM1)
SRdata$meanST1 <- as.numeric(SRdata$meanST1)

model.soil <- nls(meanSR1 ~ Fref*(1-(meanSM1-SWCopt)^2) * exp(meanST1), 
              data = SRdata,
              start = list(SWCopt = 0.25, Fref=0.75)
)








