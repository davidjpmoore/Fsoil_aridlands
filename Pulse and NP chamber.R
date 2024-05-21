# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we will devide all chamber measurements to Pulse and non-Pulse time
# for the further analysis

# Open data with chamber measurements
CH_USWkg17_20 <- read.csv("data/KN_soil_resp17_20_longHead.csv", header=TRUE, na.strings="NaN", skip=0)

#check date format
CH_USWkg17_20$date = as.Date(paste(CH_USWkg17_20$Year+2000, CH_USWkg17_20$DOY, sep = "-"), 
                             format = "%Y-%j")

# To mark pulse time we need to open "years_sum1_DM" file
pulsedefinitions<- read.csv("data/years_sum1_DM.csv", 
                            header=TRUE, na.strings = "NaN") %>%
  #retain only variables that I'm interested in.
  select(date,days_since_rain_event,max_pulse_duration, sum_R, meanSWC5, meanST5, meanGPP, meanRECO) %>%
  mutate(pulseIND = days_since_rain_event < max_pulse_duration)  %>%
  mutate(meanSWC5 = as.numeric(meanSWC5)/100) %>%
  mutate(meanST5 = as.numeric(meanST5))  %>%
  mutate(meanGPP = as.numeric(meanGPP))%>%
  #converting RECO into µmol_CO2_m2_s
  mutate(meanRECO = as.numeric(meanRECO)*(1 / 12) * (1 / 1e-6) * (1 / 86400)) 

# Combine two df
Cham_USWKG_pulsedata <- CH_USWkg17_20 %>%
  left_join(mutate(pulsedefinitions, date = as.Date(date)), by = "date")

# Rename the columns with long names

RchamberVariables= colnames(Cham_USWKG_pulsedata)
#column names of the original file contains non-standard labeling
# [1] "date"                        "Year"                        "DOY"                        
# [4] "Port.1Soil.Resp.um.co2.m2.s" "Port.1VWC"                   "Port.1Soil.Temp.deg.C"      
# [7] "Port.2soil.respum.co2.m2.s"  "Port.2VWC"                   "Port.2Soil.Temp.deg.C"      
# [10] "Port.3soil.respum.co2.m2.s"  "Port.3VWC"                   "Port.3Soil.Temp.deg.C"      
# [13] "Port.4Soil.Resp.um.co2.m2.s" "Port4VWC"                     "Port.4Soil.Temp.deg.C"      
# [16] "Port.5soil.respum.co2.m2.s"  "Port.5VWC"                   "Port.5Soil.Temp.deg.C"      
# [19] "Port.6soil.respum.co2.m2.s"  "Port.6VWC"                   "Port.6Soil.Temp.deg.C"      
# [22] "Port.7soil.respum.co2.m2.s"  "Port.7VWC"                   "Port.7Soil.Temp.deg.C"      
# [25] "days_since_rain_event"       "max_pulse_duration"          "sum_R"                      
# [28] "meanSWC5"                    "meanST5"                     "meanGPP"                    
# [31] "pulseIND"

#Create new column names
#
#.    WARNING THIS CHANGES THE COLUMN NAMES BASED ON THE ORDER THEY ARE READ
#          CHECK THEM!

newnames = c("Year",	"DOY",	"Rsoil1",	"VWC1",	"Tsoil1",	"Rsoil2",	"VWC2",	"Tsoil2",	
             "Rsoil3",	"VWC3",	"Tsoil3",	"Rsoil4",	"VWC4",	"Tsoil4",	 "Rsoil5",	"VWC5",	"Tsoil5",	
             "Rsoil6",	"VWC6",	"Tsoil6",	"Rsoil7",	"VWC7",	"Tsoil7","date",	"days_since_rain_event",
             "max_pulse_duration",	"sum_R",	"meanSWC5",	"meanST5",	"meanGPP", "meanRECO",	"pulseIND")


column_units = list("years",	"days",	"µmol_CO2_m2_s",	"VWC",	"degC",	"µmol_CO2_m2_s",	"VWC",	"degC",
                    "µmol_CO2_m2_s",	 "VWC",	"degC",	"µmol_CO2_m2_s",	"VWC",	"degC",	"µmol_CO2_m2_s",	
                    "VWC",	"degC",	"µmol_CO2_m2_s",	"VWC",	"degC","days",	 "µmol_CO2_m2_s",	"VWC",	"degC",	
                    "days",	"days",	"mm",	"VWC",	"degC",	"gCmd","µmol_CO2_m2_s",	"TF")

for (col in names(column_units)) {
  Cham_USWKG_pulsedata[[col]] <- set_units(Cham_USWKG_pulsedata[[col]], column_units[[col]])
}

colnames(Cham_USWKG_pulsedata) <- newnames

Cham_USWKG_pulsedata <- Cham_USWKG_pulsedata %>%
  mutate(Rsoil1 = ifelse(Rsoil1 < 0, NA, Rsoil1),
         Rsoil1 = ifelse(is.na(Rsoil1), NA, Rsoil1),
         Rsoil2 = ifelse(Rsoil2 < 0, NA, Rsoil2),
         Rsoil2 = ifelse(is.na(Rsoil2), NA, Rsoil2),
         Rsoil3 = ifelse(Rsoil3 < 0, NA, Rsoil3),
         Rsoil3 = ifelse(is.na(Rsoil3), NA, Rsoil3),
         Rsoil4 = ifelse(Rsoil4 < 0, NA, Rsoil4),
         Rsoil4 = ifelse(is.na(Rsoil4), NA, Rsoil4),
         Rsoil5 = ifelse(Rsoil5 < 0, NA, Rsoil5),
         Rsoil5 = ifelse(is.na(Rsoil5), NA, Rsoil5),
         Rsoil6 = ifelse(Rsoil6 < 0, NA, Rsoil6),
         Rsoil6 = ifelse(is.na(Rsoil6), NA, Rsoil6),
         Rsoil7 = ifelse(Rsoil7 < 0, NA, Rsoil7),
         Rsoil7 = ifelse(is.na(Rsoil7), NA, Rsoil7))

# Devide to Pulse and non-Pulse time
Cham_USWKG_PULSETIME <- Cham_USWKG_pulsedata[Cham_USWKG_pulsedata$days_since_rain_event <= Cham_USWKG_pulsedata$max_pulse_duration, ]
Cham_USWKG_NON_PULSETIME <- Cham_USWKG_pulsedata[Cham_USWKG_pulsedata$days_since_rain_event > Cham_USWKG_pulsedata$max_pulse_duration, ]

# Calculate mean values
Cham_USWKG_pulsedata$meanRsoil <- rowMeans(Cham_USWKG_pulsedata[,c(3,6,9,12,18,21)]
                                           ,na.rm = TRUE)
Cham_USWKG_pulsedata$meanTsoil <- rowMeans(Cham_USWKG_pulsedata[,c(5,8,11, 14,17,20,23)]
                                           ,na.rm = TRUE)
Cham_USWKG_pulsedata$meanVWC <- rowMeans(Cham_USWKG_pulsedata[,c(4,7,10,13,16,19,21)]
                                           ,na.rm = TRUE)

Cham_USWKG_PULSETIME$meanRsoil <- rowMeans(Cham_USWKG_PULSETIME[,c(3,6,9,12,18,21)]
                                           ,na.rm = TRUE)
Cham_USWKG_PULSETIME$meanTsoil <- rowMeans(Cham_USWKG_PULSETIME[,c(5,8,11, 14,17,20,23)]
                                           ,na.rm = TRUE)
Cham_USWKG_PULSETIME$meanVWC <- rowMeans(Cham_USWKG_PULSETIME[,c(4,7,10,13,16,19,21)]
                                           ,na.rm = TRUE)


Cham_USWKG_NON_PULSETIME$meanRsoil <- rowMeans(Cham_USWKG_NON_PULSETIME[,c(3,6,9,12,18,21)]
                                           ,na.rm = TRUE)
Cham_USWKG_NON_PULSETIME$meanTsoil <- rowMeans(Cham_USWKG_NON_PULSETIME[,c(5,8,11, 14,17,20,23)]
                                               ,na.rm = TRUE)
Cham_USWKG_NON_PULSETIME$meanVWC <- rowMeans(Cham_USWKG_NON_PULSETIME[,c(4,7,10,13,16,19,21)]
                                               ,na.rm = TRUE)


# Make summary file for chamber measurements 
Cham_USWKG_pulsedata$DOY1 <- round(Cham_USWKG_pulsedata$DOY, digits = 0)

# all-time data frame
Sum_Chamber_all <- Cham_USWKG_pulsedata %>%
  group_by(date) %>%
  summarise(meanGPP = mean (meanGPP, na.rm=TRUE),
            meanRsoil = mean(meanRsoil, na.rm = TRUE),
            sumRain = mean(sum_R, na.rm=TRUE),
            meanTsoil = mean(meanTsoil, na.rm=TRUE),
            meanSWC = mean(meanVWC, na.rm = TRUE)
  )

Sum_Chamber_all$DOY <- yday(Sum_Chamber_all$date)
Sum_Chamber_all1 <- Sum_Chamber_all[-(1073),]

# Pulse time data frame
Sum_Chamber_P <- Cham_USWKG_PULSETIME %>%
  group_by(date) %>%
  summarise(meanGPP = mean (meanGPP, na.rm=TRUE),
            meanRsoil = mean(meanRsoil, na.rm = TRUE),
            sumRain = mean(sum_R, na.rm=TRUE),
            meanTsoil = mean(meanTsoil, na.rm=TRUE),
            meanSWC = mean(meanVWC, na.rm = TRUE)
            
  )

Sum_Chamber_P$DOY <- yday(Sum_Chamber_P$date)
Sum_Chamber_P1 <- Sum_Chamber_P[-(432),]

# Non-pulse data frame
Sum_Chamber_NP <- Cham_USWKG_NON_PULSETIME%>%
  group_by(date) %>%
  summarise(meanGPP = mean (meanGPP, na.rm=TRUE),
            meanRsoil = mean(meanRsoil, na.rm = TRUE),
            sumRain = mean(sum_R, na.rm=TRUE),
            meanTsoil = mean(meanTsoil, na.rm=TRUE),
            meanSWC = mean(meanVWC, na.rm = TRUE)
            
  )

Sum_Chamber_NP$DOY <- yday(Sum_Chamber_NP$date)
Sum_Chamber_NP1 <- Sum_Chamber_NP[-(642),]

# Save these three data frames
# Create df for Pulse and Non-pulse time
write.csv(Sum_Chamber_all1, "data/All summary chamber.csv")
write.csv(Sum_Chamber_NP1, "data/NonPulse sum chamber.csv")
write.csv(Sum_Chamber_P1, "data/Pulse sum chamber.csv")




