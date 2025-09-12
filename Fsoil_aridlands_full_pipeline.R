# =====================================================
# >>> Read_eddy.R
# =====================================================

# 16-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)


##### Read data from eddy tower for all seasons #######################

#setwd("C:\\Users\\user\\Documents\\Fsoil_aridlands")


# First file upload from the folder on your computer because it's too big

USWkg12_18 <- read.csv("C://Users/user/Documents/Fsoil data/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv", 
                       header=TRUE, na.strings="NaN", skip=0)

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




# =====================================================
# >>> Pulse and NP eddy.R
# =====================================================

# 16-05-2024
# Anastasia Makhnykina

 library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(reshape2)

### In this file we will create two data frames for Pulse and Non-pulse time ###

# Open file with all eddy-covariance data (file was created in "Read_eddy.R")
 
USWkg12_20_summary <- read.csv("data/USWkg12_20_summary.csv", 
                               header=TRUE, na.strings="NaN", skip=0)

USWkg12_20_summary$date <- as.Date(USWkg12_20_summary$date)

# sort data by date
years_sum1 <- USWkg12_20_summary %>% arrange(date)

# Initialize the pulseduration_S variable as a vector of 0's
years_sum1$pulseduration_S <- rep(0, nrow(years_sum1))

# Find the indices of rows where there is a rain event with sum_R between 5 and 10
rain_indices_S <- which(years_sum1$sum_R > 5 & years_sum1$sum_R <= 10)

# Loop through each rain event
for (i in rain_indices_S) {
  # Set pulseduration_S to 8 for 8 days after the event
  years_sum1$pulseduration_S[i:(min(i+7, nrow(years_sum1)))] <- 8
}

# Create a new variable called pulseduration_M, initialized with zeros
years_sum1$pulseduration_M <- rep(0, nrow(years_sum1))

# Loop through each element of sum_R
for (i in 1:nrow(years_sum1)) {
  
  # Check if the sum_R value is between 10 and 20
  if (years_sum1$sum_R[i] > 10 & years_sum1$sum_R[i] <= 20) {
    
    # If yes, set the pulseduration_M variable to 14 for the next 14 days
    for (j in 1:14) {
      if (i + j <= nrow(years_sum1)) { # check if within range
        years_sum1$pulseduration_M[i + j] <- 14
      }
    }
    
  }
  
}

# Create a new variable called pulseduration_L, initialized with zeros
years_sum1$pulseduration_L <- rep(0, nrow(years_sum1))

# Loop through each element of sum_R
for (i in 21:nrow(years_sum1)) {
  
  # Check if the sum_R value was less than or equal to 20 in the previous day, and greater than 20 in the current day
  if (years_sum1$sum_R[i-1] <= 20 & years_sum1$sum_R[i] > 20) {
    
    # If yes, set the pulseduration_L variable to 20 for the next 20 days
    for (j in 0:19) {
      if (i + j <= nrow(years_sum1)) { # check if within range
        years_sum1$pulseduration_L[i + j] <- 20
      }
    }
    
  }
  
}

# create a new variable called max_pulse_duration that is the maximum of pulseduration_S, pulseduration_M, and pulseduration_L
years_sum1$max_pulse_duration <- pmax(years_sum1$pulseduration_S, years_sum1$pulseduration_M, years_sum1$pulseduration_L)

# sort data by date
years_sum1 <- years_sum1 %>% arrange(date)

# Identify days with rain events
years_sum1$rain_event <- ifelse(years_sum1$sum_R > 5, 1, 0)

# Create a new column in the dataframe to store days since last rain event
years_sum1$days_since_rain_event <- 0

# Loop through each row of the data
for (i in 2:nrow(years_sum1)) {
  # If sum_R is greater than 5, set days_since_rain_event to 0
  if (years_sum1$sum_R[i] > 5) {
    years_sum1$days_since_rain_event[i] <- 0
  } else {
    # If sum_R is less than or equal to 5, increment the days_since_rain_event by 1
    years_sum1$days_since_rain_event[i] <- years_sum1$days_since_rain_event[i-1] + 1
  }
}


# Create years_sum_Pulse0 df
years_sum_Pulse0 <- years_sum1 %>%
  filter(days_since_rain_event >= max_pulse_duration)


# Create years_sum_Pulse1 df
years_sum_Pulse1 <- years_sum1 %>%
  filter(days_since_rain_event < max_pulse_duration)

# Total rain for Pulse and non-pulse time
hist(subset(years_sum_Pulse0$sum_R, years_sum_Pulse0$sum_R != 0), 
     breaks = seq(0, 60, length.out = 30),
     main = "Non-pulse time", 
     xlab = "Rainfall total", 
     ylab = "Frequency",
     col = "red", 
     border = "white",
     lty = "solid",
     ylim = c(0, 100))

hist(subset(years_sum_Pulse1$sum_R, years_sum_Pulse1$sum_R != 0), 
     breaks = seq(min(years_sum_Pulse1$sum_R), max(years_sum_Pulse1$sum_R), length.out = 30),
     main = "Pulse time", 
     xlab = "Rainfall total", 
     ylab = "Frequency",
     col = "cyan", 
     border = "white",
     lty = "solid",
     ylim = c(0, 100))

# WRITE OUT NEW FILES
write_csv(years_sum_Pulse0, "data/years_sum_Pulse0_DM.csv")
write_csv(years_sum_Pulse1, "data/years_sum_Pulse1_DM.csv")
write_csv(years_sum1, "data/years_sum1_DM.csv")






















# =====================================================
# >>> Rain and pulses eddy.R
# =====================================================

# 16-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we need to make some analysis of data we observed ###
# and plot the main dependencies and interactions ##################

#### In This file we take the figures for Fig1 and Fig 2a (max pulse size) ##########

# Open two files for analysis 
USWkg12_20_summary <- read.csv("data/USWkg12_20_summary.csv", 
                               header=TRUE, na.strings="NaN", skip=0)

# First Plot frequency of Rain events > 5 mm 
USWkg12_20_summary$observation <- 1:nrow(USWkg12_20_summary)

USWkg12_20_summary$DOY <- paste(yday(USWkg12_20_summary$date))
USWkg12_20_summary$DOY <- as.numeric(as.character(USWkg12_20_summary$DOY))
USWkg12_20_summary$bigR <- as.numeric(USWkg12_20_summary$sum_R>5)
USWkg12_20_summary$bigRmm <- as.numeric(USWkg12_20_summary$bigR)*as.numeric(USWkg12_20_summary$sum_R)
USWkg12_20_summary$year <- substr(USWkg12_20_summary$date, 1,4)
USWkg12_20_summary$year <- as.numeric(as.character(USWkg12_20_summary$year))


USW9sum <- USWkg12_20_summary%>%
  filter(bigRmm > 0)

USW9sum %>%
  #group_by(DOY) %>%
  arrange(date) %>%
  mutate(diff = observation - lag(observation, default = first(observation)))

USW9sum$diff <- USW9sum$observation - lag(USW9sum$observation, 
                                          default = first(USW9sum$observation))

USW9sum %>%
  ggplot(aes(x=bigRmm))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Rain > 5 mm')+
  ylab('Frequency')+
  xlab('Rain, mm')+
  theme_gray()+
  theme(text = element_text(size = 12))

# Plot graph for Periods without any rain
USW9sum %>%
  ggplot(aes(x=diff))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Periods without rain')+
  ylab('Frequency')+
  xlab('Days')+
  theme_gray()+
  theme(text = element_text(size = 12))





# =====================================================
# >>> Seasons and pulses.R
# =====================================================

# 13-06-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(matrixStats)



# In the next block we will create figures for pulses for 3 seasons ###
# Spring, Summer and Winter pulses - We need make the mean Values for each season #####

### USW9sum was created in the file "Rain and pulses.R" ##########


USW9sum$Season = vector(mode = 'character', length = nrow(USW9sum))
USW9sum$Season[USW9sum$DOY %in% c(1:59,305:366)] = 'Winter'
USW9sum$Season[USW9sum$DOY %in% 60:181] = 'Spring'
USW9sum$Season[USW9sum$DOY %in% 182:304] = 'Summer'

# Winter pulses
Pulse_Win <- USW9sum %>%
  filter(Season == 'Winter')

Pulse_Win$meanRECO <- as.numeric(as.character(Pulse_Win$meanRECO))

Pulse_Win %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Winter pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

Pulse2Win <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (348:365))

Pulse2Win$meanRECO <- as.numeric(as.character(Pulse2Win$meanRECO))
Pulse2Win$meanSWC5 <- as.numeric(as.character(Pulse2Win$meanSWC5))
Pulse2Win$sdReco <- as.numeric(as.character(Pulse2Win$sdReco))
Pulse2Win$sdSWC5 <- as.numeric(as.character(Pulse2Win$sdSWC5))

Pulse2Win %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanRECO),size=2)+
  geom_point(aes(y=meanSWC5/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+geom_errorbar(aes(ymin=meanRECO - sdReco, ymax= meanRECO + sdReco), width=.2,
                  position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC5/10 - sdSWC5/10, ymax= meanSWC5/10 + sdSWC5/10), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('DOY')+
  ggtitle('Winter pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))

# Pulses with Max CO2 release (more than 0.6 micromol m-2 s-1)
Pulse_Win_Max <- Pulse_Win %>%
  filter(meanRECO > 0.6)

# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxWin1 <- USWkg12_20_summary %>%
  filter(year == 2013) %>%
  filter(DOY %in% (323:340))

MaxWin2 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (316:333))

MaxWin3 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (343:360))

MaxWin4 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (29:46))

# Just not enough days in one year to finish this pulse
MaxWin5 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (354:366))

MaxWin6 <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (348:365))

# The next two pulses may overlap each other
MaxWin7 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (320:337))

MaxWin8 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (322:339))

MaxWin9 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (340:357))


# To exclude MaxWin5 and make the one df for 8 Reco and SWC5
MaxWin <- MaxWin1 %>%
  select(meanRECO, sdReco, meanSWC5,sdSWC5, DOY)

MaxWin$Reco2 <- MaxWin2$meanRECO
MaxWin$Reco2sd <- MaxWin2$sdReco
MaxWin$SWC2 <- MaxWin2$meanSWC5
MaxWin$SWC2sd <- MaxWin2$sdSWC5

MaxWin$Reco3 <- MaxWin3$meanRECO
MaxWin$Reco3sd <- MaxWin3$sdReco
MaxWin$SWC3 <- MaxWin3$meanSWC5
MaxWin$SWC3sd <- MaxWin3$sdSWC5

MaxWin$Reco4 <- MaxWin4$meanRECO
MaxWin$Reco4sd <- MaxWin4$sdReco
MaxWin$SWC4 <- MaxWin4$meanSWC5
MaxWin$SWC4sd <- MaxWin4$sdSWC5

MaxWin$Reco5 <- MaxWin6$meanRECO
MaxWin$Reco5sd <- MaxWin6$sdReco
MaxWin$SWC5 <- MaxWin6$meanSWC5
MaxWin$SWC5sd <- MaxWin6$sdSWC5

MaxWin$Reco6 <- MaxWin7$meanRECO
MaxWin$Reco6sd <- MaxWin7$sdReco
MaxWin$SWC6 <- MaxWin7$meanSWC5
MaxWin$SWC6sd <- MaxWin7$sdSWC5

MaxWin$Reco7 <- MaxWin8$meanRECO
MaxWin$Reco7sd <- MaxWin8$sdReco
MaxWin$SWC7 <- MaxWin8$meanSWC5
MaxWin$SWC7sd <- MaxWin8$sdSWC5

MaxWin$Reco8 <- MaxWin9$meanRECO
MaxWin$Reco8sd <- MaxWin9$sdReco
MaxWin$SWC8 <- MaxWin9$meanSWC5
MaxWin$SWC8sd <- MaxWin9$sdSWC5

# make them numeric
MaxWin$meanRECO <- as.numeric(as.character(MaxWin$meanRECO))
MaxWin$Reco2 <- as.numeric(as.character(MaxWin$Reco2))
MaxWin$Reco3 <- as.numeric(as.character(MaxWin$Reco3))
MaxWin$Reco4 <- as.numeric(as.character(MaxWin$Reco4))
MaxWin$Reco5 <- as.numeric(as.character(MaxWin$Reco5))
MaxWin$Reco6 <- as.numeric(as.character(MaxWin$Reco6))
MaxWin$Reco7 <- as.numeric(as.character(MaxWin$Reco7))
MaxWin$Reco8 <- as.numeric(as.character(MaxWin$Reco8))

MaxWin$meanFlux <- rowMeans(MaxWin[,c(1,6,10,14,18,22,26,30)], na.rm=TRUE)

#MaxWin$meanSRsd <- rowSds(MaxWin[,c(1,5,9,13,17,21,25,29)], na.rm=TRUE)

MaxWin$SD_Flux = vector(mode = 'character', length = nrow(MaxWin))
MaxWin$SD_Flux[MaxWin$DOY %in% 323] = 0.2
MaxWin$SD_Flux[MaxWin$DOY %in% 324] = 0.26
MaxWin$SD_Flux[MaxWin$DOY %in% 325] = 0.33
MaxWin$SD_Flux[MaxWin$DOY %in% 326] = 0.08
MaxWin$SD_Flux[MaxWin$DOY %in% 327] = 0.19
MaxWin$SD_Flux[MaxWin$DOY %in% 328] = 0.18
MaxWin$SD_Flux[MaxWin$DOY %in% 329] = 0.16
MaxWin$SD_Flux[MaxWin$DOY %in% 330] = 0.18
MaxWin$SD_Flux[MaxWin$DOY %in% 331] = 0.21
MaxWin$SD_Flux[MaxWin$DOY %in% 332] = 0.19
MaxWin$SD_Flux[MaxWin$DOY %in% 333] = 0.16
MaxWin$SD_Flux[MaxWin$DOY %in% 334] = 0.19
MaxWin$SD_Flux[MaxWin$DOY %in% 335] = 0.3
MaxWin$SD_Flux[MaxWin$DOY %in% 336] = 0.21
MaxWin$SD_Flux[MaxWin$DOY %in% 337] = 0.18
MaxWin$SD_Flux[MaxWin$DOY %in% 338] = 0.17
MaxWin$SD_Flux[MaxWin$DOY %in% 339] = 0.11
MaxWin$SD_Flux[MaxWin$DOY %in% 340] = 0.11

MaxWin$SD_Flux <- as.numeric(as.character(MaxWin$SD_Flux))


MaxWin$meanSWC5 <- as.numeric(as.character(MaxWin$meanSWC5))
MaxWin$SWC2 <- as.numeric(as.character(MaxWin$SWC2))
MaxWin$SWC3 <- as.numeric(as.character(MaxWin$SWC3))
MaxWin$SWC4 <- as.numeric(as.character(MaxWin$SWC4))
MaxWin$SWC5 <- as.numeric(as.character(MaxWin$SWC5))
MaxWin$SWC6 <- as.numeric(as.character(MaxWin$SWC6))
MaxWin$SWC7 <- as.numeric(as.character(MaxWin$SWC7))
MaxWin$SWC8 <- as.numeric(as.character(MaxWin$SWC8))

MaxWin$meanSWC <- rowMeans(MaxWin[,c(3,8,12,16,20,24,28,32)], na.rm=TRUE)

MaxWin$DOY <- MaxWin1$DOY 

MaxWin$SD_SWC = vector(mode = 'character', length = nrow(MaxWin))
MaxWin$SD_SWC[MaxWin$DOY %in% 323] = 4.35
MaxWin$SD_SWC[MaxWin$DOY %in% 324] = 4.08
MaxWin$SD_SWC[MaxWin$DOY %in% 325] = 4.76
MaxWin$SD_SWC[MaxWin$DOY %in% 326] = 7.49
MaxWin$SD_SWC[MaxWin$DOY %in% 327] = 4.54
MaxWin$SD_SWC[MaxWin$DOY %in% 328] = 3.72
MaxWin$SD_SWC[MaxWin$DOY %in% 329] = 2.49
MaxWin$SD_SWC[MaxWin$DOY %in% 330] = 2.08
MaxWin$SD_SWC[MaxWin$DOY %in% 331] = 1.88
MaxWin$SD_SWC[MaxWin$DOY %in% 332] = 3.53
MaxWin$SD_SWC[MaxWin$DOY %in% 333] = 4.19
MaxWin$SD_SWC[MaxWin$DOY %in% 334] = 4.97
MaxWin$SD_SWC[MaxWin$DOY %in% 335] = 4.8
MaxWin$SD_SWC[MaxWin$DOY %in% 336] = 4.63
MaxWin$SD_SWC[MaxWin$DOY %in% 337] = 3.48
MaxWin$SD_SWC[MaxWin$DOY %in% 338] = 3.04
MaxWin$SD_SWC[MaxWin$DOY %in% 339] = 2.92
MaxWin$SD_SWC[MaxWin$DOY %in% 340] = 2.66

MaxWin$SD_SWC <- as.numeric(as.character(MaxWin$SD_SWC))


MaxWin$Pulse_day = vector(mode = 'character', length = nrow(MaxWin))
MaxWin$Pulse_day[MaxWin$DOY %in% 323] = '-3'
MaxWin$Pulse_day[MaxWin$DOY %in% 324] = '-2'
MaxWin$Pulse_day[MaxWin$DOY %in% 325] = '-1'
MaxWin$Pulse_day[MaxWin$DOY %in% 326] = '0'
MaxWin$Pulse_day[MaxWin$DOY %in% 327] = '1'
MaxWin$Pulse_day[MaxWin$DOY %in% 328] = '2'
MaxWin$Pulse_day[MaxWin$DOY %in% 329] = '3'
MaxWin$Pulse_day[MaxWin$DOY %in% 330] = '4'
MaxWin$Pulse_day[MaxWin$DOY %in% 331] = '5'
MaxWin$Pulse_day[MaxWin$DOY %in% 332] = '6'
MaxWin$Pulse_day[MaxWin$DOY %in% 333] = '7'
MaxWin$Pulse_day[MaxWin$DOY %in% 334] = '8'
MaxWin$Pulse_day[MaxWin$DOY %in% 335] = '9'
MaxWin$Pulse_day[MaxWin$DOY %in% 336] = '10'
MaxWin$Pulse_day[MaxWin$DOY %in% 337] = '11'
MaxWin$Pulse_day[MaxWin$DOY %in% 338] = '12'
MaxWin$Pulse_day[MaxWin$DOY %in% 339] = '13'
MaxWin$Pulse_day[MaxWin$DOY %in% 340] = '14'



################# Graph with Standard Errors!!!! #####################
MaxWin %>%
  ggplot(aes(x=DOY))+
  geom_point(aes(y = meanFlux),size=2)+
  geom_point(aes(y=meanSWC/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanFlux - (SD_Flux/sqrt(8)), ymax= meanFlux + (SD_Flux/sqrt(8))), width=.2,
                  position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/10 - ((SD_SWC/10)/sqrt(8)), ymax= meanSWC/10 + ((SD_SWC/10)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('DOY')+
  ggtitle('Mean Winter pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))

MaxWin$Pulse_day <- as.numeric(as.character(MaxWin$Pulse_day))

MaxWin %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanFlux),size=2)+
  geom_point(aes(y=meanSWC/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanFlux - (SD_Flux/sqrt(8)), ymax= meanFlux + (SD_Flux/sqrt(8))), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/10 - ((SD_SWC/10)/sqrt(8)), ymax= meanSWC/10 + ((SD_SWC/10)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('Pulse duration, days')+
  #xlim(-3,14)+
  ggtitle('Mean Winter pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))


# Another classification - use the biggest rain events - more than 20 mm  


# Spring pulses
### We need 2 dfs - USW9sum and USWkg12_20_summary

Pulse_Spr <- USW9sum %>%
  filter(Season == 'Spring')

Pulse_Spr$meanRECO <- as.numeric(as.character(Pulse_Spr$meanRECO))

Pulse_Spr %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Spring pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

# Pulses with Max CO2 release (more than 0.6 micromol m-2 s-1)
Pulse_Spr_Max <- Pulse_Spr %>%
  filter(meanRECO > 1)

# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxSpr1 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (99:116))

MaxSpr2 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (174:191))

MaxSpr3 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (175:192))

MaxSpr4 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (178:195))

MaxSpr5 <- USWkg12_20_summary %>%
  filter(year == 2017) %>%
  filter(DOY %in% (173:190))

MaxSpr6 <- USWkg12_20_summary %>%
  filter(year == 2018) %>%
  filter(DOY %in% (164:181))

# Making one document 
MaxSpr <- MaxSpr1 %>%
  select(meanRECO, sdReco, meanSWC5,sdSWC5)

MaxSpr$Reco2 <- MaxSpr2$meanRECO
MaxSpr$Reco2sd <- MaxSpr2$sdReco
MaxSpr$SWC2 <- MaxSpr2$meanSWC5
MaxSpr$SWC2sd <- MaxSpr2$sdSWC5

MaxSpr$Reco3 <- MaxSpr3$meanRECO
MaxSpr$Reco3sd <- MaxSpr3$sdReco
MaxSpr$SWC3 <- MaxSpr3$meanSWC5
MaxSpr$SWC3sd <- MaxSpr3$sdSWC5

MaxSpr$Reco4 <- MaxSpr4$meanRECO
MaxSpr$Reco4sd <- MaxSpr4$sdReco
MaxSpr$SWC4 <- MaxSpr4$meanSWC5
MaxSpr$SWC4sd <- MaxSpr4$sdSWC5

MaxSpr$Reco5 <- MaxSpr5$meanRECO
MaxSpr$Reco5sd <- MaxSpr5$sdReco
MaxSpr$SWC5 <- MaxSpr5$meanSWC5
MaxSpr$SWC5sd <- MaxSpr5$sdSWC5

MaxSpr$Reco6 <- MaxSpr6$meanRECO
MaxSpr$Reco6sd <- MaxSpr6$sdReco
MaxSpr$SWC6 <- MaxSpr6$meanSWC5
MaxSpr$SWC6sd <- MaxSpr6$sdSWC5

MaxSpr$meanRECO <- as.numeric(as.character(MaxSpr$meanRECO))
MaxSpr$Reco2 <- as.numeric(as.character(MaxSpr$Reco2))
MaxSpr$Reco3 <- as.numeric(as.character(MaxSpr$Reco3))
MaxSpr$Reco4 <- as.numeric(as.character(MaxSpr$Reco4))
MaxSpr$Reco5 <- as.numeric(as.character(MaxSpr$Reco5))
MaxSpr$Reco6 <- as.numeric(as.character(MaxSpr$Reco6))

MaxSpr$meanSWC5 <- as.numeric(as.character(MaxSpr$meanSWC5))
MaxSpr$SWC2 <- as.numeric(as.character(MaxSpr$SWC2))
MaxSpr$SWC3 <- as.numeric(as.character(MaxSpr$SWC3))
MaxSpr$SWC4 <- as.numeric(as.character(MaxSpr$SWC4))
MaxSpr$SWC5 <- as.numeric(as.character(MaxSpr$SWC5))
MaxSpr$SWC6 <- as.numeric(as.character(MaxSpr$SWC6))


##### 
MaxSpr$meanFlux <- rowMeans(MaxSpr[,c(1,5,9,13,17,21)], na.rm=TRUE)
MaxSpr$meanSWC <- rowMeans(MaxSpr[,c(3,7,11,15,19,23)], na.rm=TRUE)

MaxSpr$DOY <- MaxSpr1$DOY 

MaxSpr$Pulse_day = vector(mode = 'character', length = nrow(MaxSpr))
MaxSpr$Pulse_day[MaxSpr$DOY %in% 99] = '-3'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 100] = '-2'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 101] = '-1'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 102] = '0'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 103] = '1'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 104] = '2'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 105] = '3'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 106] = '4'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 107] = '5'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 108] = '6'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 109] = '7'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 110] = '8'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 111] = '9'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 112] = '10'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 113] = '11'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 114] = '12'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 115] = '13'
MaxSpr$Pulse_day[MaxSpr$DOY %in% 116] = '14'

MaxSpr$Pulse_day <- as.numeric(as.character(MaxSpr$Pulse_day))

MaxSpr$SD_Flux = vector(mode = 'character', length = nrow(MaxSpr))
MaxSpr$SD_Flux[MaxSpr$DOY %in% 99] = 0.26
MaxSpr$SD_Flux[MaxSpr$DOY %in% 100] = 0.33
MaxSpr$SD_Flux[MaxSpr$DOY %in% 101] = 0.54
MaxSpr$SD_Flux[MaxSpr$DOY %in% 102] = 0.79
MaxSpr$SD_Flux[MaxSpr$DOY %in% 103] = 0.69
MaxSpr$SD_Flux[MaxSpr$DOY %in% 104] = 0.63
MaxSpr$SD_Flux[MaxSpr$DOY %in% 105] = 0.43
MaxSpr$SD_Flux[MaxSpr$DOY %in% 106] = 0.41
MaxSpr$SD_Flux[MaxSpr$DOY %in% 107] = 0.68
MaxSpr$SD_Flux[MaxSpr$DOY %in% 108] = 1.06
MaxSpr$SD_Flux[MaxSpr$DOY %in% 109] = 1.27
MaxSpr$SD_Flux[MaxSpr$DOY %in% 110] = 1.18
MaxSpr$SD_Flux[MaxSpr$DOY %in% 111] = 0.99
MaxSpr$SD_Flux[MaxSpr$DOY %in% 112] = 0.9
MaxSpr$SD_Flux[MaxSpr$DOY %in% 113] = 0.78
MaxSpr$SD_Flux[MaxSpr$DOY %in% 114] = 0.67
MaxSpr$SD_Flux[MaxSpr$DOY %in% 115] = 0.72
MaxSpr$SD_Flux[MaxSpr$DOY %in% 116] = 0.75

MaxSpr$SD_Flux <- as.numeric(as.character(MaxSpr$SD_Flux))

MaxSpr$SD_SWC = vector(mode = 'character', length = nrow(MaxSpr))
MaxSpr$SD_SWC[MaxSpr$DOY %in% 99] = 2.06
MaxSpr$SD_SWC[MaxSpr$DOY %in% 100] = 2.3
MaxSpr$SD_SWC[MaxSpr$DOY %in% 101] = 2.17
MaxSpr$SD_SWC[MaxSpr$DOY %in% 102] = 6.7
MaxSpr$SD_SWC[MaxSpr$DOY %in% 103] = 4.91
MaxSpr$SD_SWC[MaxSpr$DOY %in% 104] = 5.65
MaxSpr$SD_SWC[MaxSpr$DOY %in% 105] = 4.66
MaxSpr$SD_SWC[MaxSpr$DOY %in% 106] = 4.29
MaxSpr$SD_SWC[MaxSpr$DOY %in% 107] = 4
MaxSpr$SD_SWC[MaxSpr$DOY %in% 108] = 5.18
MaxSpr$SD_SWC[MaxSpr$DOY %in% 109] = 4.76
MaxSpr$SD_SWC[MaxSpr$DOY %in% 110] = 4.95
MaxSpr$SD_SWC[MaxSpr$DOY %in% 111] = 6.21
MaxSpr$SD_SWC[MaxSpr$DOY %in% 112] = 5.06
MaxSpr$SD_SWC[MaxSpr$DOY %in% 113] = 4.35
MaxSpr$SD_SWC[MaxSpr$DOY %in% 114] = 3.98
MaxSpr$SD_SWC[MaxSpr$DOY %in% 115] = 3.43
MaxSpr$SD_SWC[MaxSpr$DOY %in% 116] = 4.22

MaxSpr$SD_SWC <- as.numeric(as.character(MaxSpr$SD_SWC))

MaxSpr %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanFlux),size=2)+
  geom_point(aes(y=meanSWC/15), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanFlux - (SD_Flux/sqrt(8)), ymax= meanFlux + (SD_Flux/sqrt(8))), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/15 - ((SD_SWC/15)/sqrt(8)), ymax= meanSWC/15 + ((SD_SWC/15)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('Pulse duration, days')+
  #ylim(0,4)+
  ggtitle('Mean Spring pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*15, 
                                          name="SWC 5 cm , %"))


# Summer pulses
Pulse_Sum <- USW9sum %>%
  filter(Season == 'Summer')

Pulse_Sum$meanRECO <- as.numeric(as.character(Pulse_Sum$meanRECO))

Pulse_Sum %>%
  ggplot(aes(x=meanRECO))+
  geom_histogram(color="black", fill="white")+
  ggtitle('Summer pulses')+
  ylab('Frequency')+
  xlab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_gray()+
  theme(text = element_text(size = 12))

# Pulses with Max CO2 release (more than 0.6 micromol m-2 s-1)
Pulse_Sum_Max <- Pulse_Sum %>%
  filter(meanRECO > 2.7)

# Take 9 pulses with 3 days before and 14 days after rain pulse
MaxSum1 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (181:198))

MaxSum2 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (183:200))

MaxSum3 <- USWkg12_20_summary %>%
  filter(year == 2015) %>%
  filter(DOY %in% (209:226))

MaxSum4 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (196:213))

MaxSum5 <- USWkg12_20_summary %>%
  filter(year == 2016) %>%
  filter(DOY %in% (197:214))

MaxSum6 <- USWkg12_20_summary %>%
  filter(year == 2018) %>%
  filter(DOY %in% (216:233))

MaxSum7 <- USWkg12_20_summary %>%
  filter(year == 2019) %>%
  filter(DOY %in% (237:254))

# Making one document 
MaxSum <- MaxSum1 %>%
  select(meanRECO, sdReco, meanSWC5,sdSWC5)

MaxSum$Reco2 <- MaxSum2$meanRECO
MaxSum$Reco2sd <- MaxSum2$sdReco
MaxSum$SWC2 <- MaxSum2$meanSWC5
MaxSum$SWC2sd <- MaxSum2$sdSWC5

MaxSum$Reco3 <- MaxSum3$meanRECO
MaxSum$Reco3sd <- MaxSum3$sdReco
MaxSum$SWC3 <- MaxSum3$meanSWC5
MaxSum$SWC3sd <- MaxSpr3$sdSWC5

MaxSum$Reco4 <- MaxSum4$meanRECO
MaxSum$Reco4sd <- MaxSum4$sdReco
MaxSum$SWC4 <- MaxSum4$meanSWC5
MaxSum$SWC4sd <- MaxSum4$sdSWC5

MaxSum$Reco5 <- MaxSum5$meanRECO
MaxSum$Reco5sd <- MaxSum5$sdReco
MaxSum$SWC5 <- MaxSum5$meanSWC5
MaxSum$SWC5sd <- MaxSum5$sdSWC5

MaxSum$Reco6 <- MaxSum6$meanRECO
MaxSum$Reco6sd <- MaxSum6$sdReco
MaxSum$SWC6 <- MaxSum6$meanSWC5
MaxSum$SWC6sd <- MaxSum6$sdSWC5

MaxSum$Reco7 <- MaxSum7$meanRECO
MaxSum$Reco7sd <- MaxSum7$sdReco
MaxSum$SWC7 <- MaxSum7$meanSWC5
MaxSum$SWC7sd <- MaxSum7$sdSWC5

MaxSum$meanRECO <- as.numeric(as.character(MaxSum$meanRECO))
MaxSum$Reco2 <- as.numeric(as.character(MaxSum$Reco2))
MaxSum$Reco3 <- as.numeric(as.character(MaxSum$Reco3))
MaxSum$Reco4 <- as.numeric(as.character(MaxSum$Reco4))
MaxSum$Reco5 <- as.numeric(as.character(MaxSum$Reco5))
MaxSum$Reco6 <- as.numeric(as.character(MaxSum$Reco6))
MaxSum$Reco7 <- as.numeric(as.character(MaxSum$Reco7))

MaxSum$meanSWC5 <- as.numeric(as.character(MaxSum$meanSWC5))
MaxSum$SWC2 <- as.numeric(as.character(MaxSum$SWC2))
MaxSum$SWC3 <- as.numeric(as.character(MaxSum$SWC3))
MaxSum$SWC4 <- as.numeric(as.character(MaxSum$SWC4))
MaxSum$SWC5 <- as.numeric(as.character(MaxSum$SWC5))
MaxSum$SWC6 <- as.numeric(as.character(MaxSum$SWC6))
MaxSum$SWC7 <- as.numeric(as.character(MaxSum$SWC7))

##########

MaxSum$meanFlux <- rowMeans(MaxSum[,c(1,5,9,13,17,21,25)], na.rm=TRUE)
MaxSum$meanSWC <- rowMeans(MaxSum[,c(3,7,11,15,19,23,27)], na.rm=TRUE)

MaxSum$DOY <- MaxSum1$DOY 

MaxSum$Pulse_day = vector(mode = 'character', length = nrow(MaxSum))
MaxSum$Pulse_day[MaxSum$DOY %in% 181] = '-3'
MaxSum$Pulse_day[MaxSum$DOY %in% 182] = '-2'
MaxSum$Pulse_day[MaxSum$DOY %in% 183] = '-1'
MaxSum$Pulse_day[MaxSum$DOY %in% 184] = '0'
MaxSum$Pulse_day[MaxSum$DOY %in% 185] = '1'
MaxSum$Pulse_day[MaxSum$DOY %in% 186] = '2'
MaxSum$Pulse_day[MaxSum$DOY %in% 187] = '3'
MaxSum$Pulse_day[MaxSum$DOY %in% 188] = '4'
MaxSum$Pulse_day[MaxSum$DOY %in% 189] = '5'
MaxSum$Pulse_day[MaxSum$DOY %in% 190] = '6'
MaxSum$Pulse_day[MaxSum$DOY %in% 191] = '7'
MaxSum$Pulse_day[MaxSum$DOY %in% 192] = '8'
MaxSum$Pulse_day[MaxSum$DOY %in% 193] = '9'
MaxSum$Pulse_day[MaxSum$DOY %in% 194] = '10'
MaxSum$Pulse_day[MaxSum$DOY %in% 195] = '11'
MaxSum$Pulse_day[MaxSum$DOY %in% 196] = '12'
MaxSum$Pulse_day[MaxSum$DOY %in% 197] = '13'
MaxSum$Pulse_day[MaxSum$DOY %in% 198] = '14'

MaxSum$Pulse_day <- as.numeric(as.character(MaxSum$Pulse_day))

MaxSum$SD_Flux = vector(mode = 'character', length = nrow(MaxSum))
MaxSum$SD_Flux[MaxSum$DOY %in% 181] = 0.36
MaxSum$SD_Flux[MaxSum$DOY %in% 182] = 0.52
MaxSum$SD_Flux[MaxSum$DOY %in% 183] = 0.39
MaxSum$SD_Flux[MaxSum$DOY %in% 184] = 0.29
MaxSum$SD_Flux[MaxSum$DOY %in% 185] = 0.45
MaxSum$SD_Flux[MaxSum$DOY %in% 186] = 0.49
MaxSum$SD_Flux[MaxSum$DOY %in% 187] = 0.54
MaxSum$SD_Flux[MaxSum$DOY %in% 188] = 0.4
MaxSum$SD_Flux[MaxSum$DOY %in% 189] = 0.36
MaxSum$SD_Flux[MaxSum$DOY %in% 190] = 0.43
MaxSum$SD_Flux[MaxSum$DOY %in% 191] = 0.51
MaxSum$SD_Flux[MaxSum$DOY %in% 192] = 0.6
MaxSum$SD_Flux[MaxSum$DOY %in% 193] = 0.58
MaxSum$SD_Flux[MaxSum$DOY %in% 194] = 0.55
MaxSum$SD_Flux[MaxSum$DOY %in% 195] = 0.55
MaxSum$SD_Flux[MaxSum$DOY %in% 196] = 0.59
MaxSum$SD_Flux[MaxSum$DOY %in% 197] = 0.5
MaxSum$SD_Flux[MaxSum$DOY %in% 198] = 0.42

MaxSum$SD_Flux <- as.numeric(as.character(MaxSum$SD_Flux))

MaxSum$SD_SWC = vector(mode = 'character', length = nrow(MaxSum))
MaxSum$SD_SWC[MaxSum$DOY %in% 181] = 2.35
MaxSum$SD_SWC[MaxSum$DOY %in% 182] = 5.13
MaxSum$SD_SWC[MaxSum$DOY %in% 183] = 3.35
MaxSum$SD_SWC[MaxSum$DOY %in% 184] = 6.79
MaxSum$SD_SWC[MaxSum$DOY %in% 185] = 4.89
MaxSum$SD_SWC[MaxSum$DOY %in% 186] = 4.45
MaxSum$SD_SWC[MaxSum$DOY %in% 187] = 3.72
MaxSum$SD_SWC[MaxSum$DOY %in% 188] = 3.57
MaxSum$SD_SWC[MaxSum$DOY %in% 189] = 3.51
MaxSum$SD_SWC[MaxSum$DOY %in% 190] = 3.28
MaxSum$SD_SWC[MaxSum$DOY %in% 191] = 3.25
MaxSum$SD_SWC[MaxSum$DOY %in% 192] = 4.34
MaxSum$SD_SWC[MaxSum$DOY %in% 193] = 3.92
MaxSum$SD_SWC[MaxSum$DOY %in% 194] = 3.49
MaxSum$SD_SWC[MaxSum$DOY %in% 195] = 4.31
MaxSum$SD_SWC[MaxSum$DOY %in% 196] = 3.82
MaxSum$SD_SWC[MaxSum$DOY %in% 197] = 4.06
MaxSum$SD_SWC[MaxSum$DOY %in% 198] = 4.47

MaxSum$SD_SWC <- as.numeric(as.character(MaxSum$SD_SWC))

MaxSum %>%
  ggplot(aes(x=as.factor(Pulse_day)))+
  geom_point(aes(y = meanFlux),size=2)+
  geom_point(aes(y=meanSWC/10), color = 'blue', size=2)+
  theme(text = element_text(size = 15), plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 2, linetype = "solid")
        #plot.background = element_rect(fill = "#BFD5E3")
  )+
  geom_errorbar(aes(ymin=meanFlux - (SD_Flux/sqrt(8)), ymax= meanFlux + (SD_Flux/sqrt(8))), width=.2,
                position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=meanSWC/10 - ((SD_SWC/10)/sqrt(8)), ymax= meanSWC/10 + ((SD_SWC/10)/sqrt(8))), 
                width=.2, position=position_dodge(.9))+
  #theme_get()+
  xlab('Pulse duration, days')+
  #ylim(0,4)+
  ggtitle('Mean Summer pulse')+
  scale_y_continuous(name=~paste("Reco, ", mu, "mol m"^-2,"s"^-1),
                     sec.axis = sec_axis( trans=~.*10, 
                                          name="SWC 5 cm , %"))








# =====================================================
# >>> GPP vs Reco eddy.R
# =====================================================

# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(ggpubr)

# In this file we focus on the interactions Between Reco and GPP fluxes ###
# for all time, Pulse and non-Pulse time ##################################


# Read pulse division docs
years_sum1 <- read.csv("data/years_sum1_DM.csv")
years_sum_Pulse0 <- read.csv("data/years_sum_Pulse0_DM.csv")
years_sum_Pulse1 <- read.csv("data/years_sum_Pulse1_DM.csv")


# Plot data For All time
years_sum1 %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('All time')+
  ylim(0,6)

# Plot data for pulse time
years_sum_Pulse1 %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Pulse time')+
  ylim(0,6)

# Plot data for non-Pulse time
years_sum_Pulse0 %>%
  ggplot(aes(x=meanGPP, y = meanRECO))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Non-pulse time')+
  ylim(0,6)+
  xlim(0,6)






# =====================================================
# >>> SWC and ST space.R
# =====================================================

# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we plot the Reco in SWC-Soil temperature space ###
# for Pulse and non-Pulse time ##################################


# Read pulse division docs
years_sum1 <- read.csv("data/years_sum1_DM.csv")
years_sum_Pulse0 <- read.csv("data/years_sum_Pulse0_DM.csv")
years_sum_Pulse1 <- read.csv("data/years_sum_Pulse1_DM.csv")


# # We need to choose the soil depth 
# 
# cor.test(summary_P$meanRECO, summary_P$meanSWC5)
# 
# 
# cor.test(years_sum_Pulse0$meanRECO, years_sum_Pulse0$meanSWC5)
# #       cor 
# # 0.4208387 
# cor.test(summary_P$meanRECO, summary_P$meanSWC15)
# #       cor 
# # 0.1847413 

# For paper - let's choose 5 cm depth-graphs



# Plot the same for the 5 cm depth
plot(years_sum_Pulse1$meanSWC5, years_sum_Pulse1$meanST5, 
     cex = years_sum_Pulse1$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC 5 cm, %", # add x-axis label
     ylab = "Mean Tsoil 5 cm, °C", # add y-axis label
     main = "SWC and Soil T - during pulses", # add plot title
     xlim=c(0,45),
     ylim=c(0,40))

plot(years_sum_Pulse0$meanSWC5, years_sum_Pulse0$meanST5, 
     cex = years_sum_Pulse0$meanRECO/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC 5 cm, %", # add x-axis label
     ylab = "Mean Tsoil 5 cm, °C", # add y-axis label
     main = "SWC and Soil T - between pulses" , # add plot title
     xlim=c(0,45),
     ylim=c(0,40))













# =====================================================
# >>> 15% Threshold.R
# =====================================================

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(stats)
library(grDevices)
library(readr)
library(ggpubr)
library(minpack.lm)


# Read other pulse division docs - with "DM" in their names
years_sum1 <- read.csv("data/years_sum1_DM.csv")
years_sum1 <- na.omit(years_sum1)
years_sum1$date <- as.Date(years_sum1$date)

plot(years_sum1$meanSWC5, years_sum1$meanRECO)

years_sum2 <- years_sum1 %>%
  select(c(1:19 | 23))


#### Divide df years_sum1 to two dfs based on different moisture values ########
### We choose moisture levels from 5 to 15 % for SWC5 

years_sum2$Threshold_15 <- years_sum2$meanSWC5 >=15
years_sum2$Threshold_15 <- as.numeric(as.logical(years_sum2$Threshold_15))


################ Divide th whole df to two groups and make modelling procedure without pulse duration things 

############################################
# 15% Threshold #############################
############################################

years_sum2_15more <- years_sum2 %>%
  filter(Threshold_15 == 1)

years_sum2_15less <- years_sum2 %>%
  filter(Threshold_15 == 0)

# Assign variables
SoilMoisture_15 <- years_sum2_15less$meanSWC5/100

meanSWC5_NP_15 <- SoilMoisture_15
meanST5_NP_15 <- years_sum2_15less$meanST5
meanGPP_NP_15 <- years_sum2_15less$meanGPP
GPPmax_NP_15 <- max(years_sum2_15less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP_15 <- nls(meanRECO ~ FrefNP_15*((meanGPP_NP_15/GPPmax_NP_15 +n)/1+n) *
                            (1-c4*(0.1-meanSWC5_NP_15)^2)*exp(b4*meanST5_NP_15), 
                          data = years_sum2_15less,
                          start = list(FrefNP_15=0.75, c4=56.54, b4=0.04, n=0.84),
                          control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP_15 = summary(Param_model4_NP_15)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.896844   0.036463  24.596  < 2e-16 ***
#  c4     35.724636   4.772225   7.486 1.04e-13 ***
#  b4      0.040524   0.001451  27.930  < 2e-16 ***
#  n       0.083102   0.002915  28.511  < 2e-16 ***

FrefNP_15 = 0.896844
SMoptNP_15 =0.125 
c4NP_15 = 35.724636
b4NP_15 =  0.040524
nNP_15=   0.083102

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P_15 = years_sum2_15more$meanSWC5/100
meanSWC5_P_15 = SoilMoisture_P_15
meanST5_P_15 = years_sum2_15more$meanST5
meanGPP_P_15 = years_sum2_15more$meanGPP
GPPmax_P_15 = max(years_sum2_15more$meanGPP, na.rm=TRUE)

Param_model4_P_15 <- nls(meanRECO ~ FrefP_15*((meanGPP_P_15/GPPmax_P_15 +n)/1+n) *(1-c4*(0.1-meanSWC5_P_15)^2)*exp(b4*meanST5_P_15), 
                         data = years_sum2_15more,
                         start = list(FrefP_15=0.75, c4=56.54, b4=0.04, n=0.84),
                         control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P_15 = summary(Param_model4_P_15)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP  0.317582   0.037347   8.504  < 2e-16 ***
#  c4    -9.670967   2.076913  -4.656 3.98e-06 ***
#  b4     0.060625   0.003162  19.173  < 2e-16 ***
#  n      0.400895   0.037033  10.825  < 2e-16 ***

# Pulse parameters
FrefP_15 =  0.317582
SMoptP_15 =0.125 
c4P_15 = -9.670967   
b4P_15 =  0.060625
nP_15 = 0.400895


########## Fit All-time model ##################

# Setting up drivers for all time
All_meanSWC5_15 = years_sum1$meanSWC5/100
All_meanST5_15 = years_sum1$meanST5
All_meanGPP_15 = years_sum1$meanGPP
All_GPPmax_15 = max(years_sum1$meanGPP, na.rm = TRUE)

Param_model4_All_15 <- nls(meanRECO ~ FrefL*((All_meanGPP_15/All_GPPmax_15 +nL)/1+nL) *(1-c4L*(0.1-All_meanSWC5_15)^2)*exp(b4L*All_meanST5_15), 
                           data = years_sum1,
                           start = list(FrefL=0.75,  c4L=56.54, b4L=0.04, nL=0.84),
                           control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_All_15 = summary(Param_model4_All_15)

#Parameters:
#  Estimate Std. Error t value Pr(>|t|)    
#FrefL  1.136042   0.049023  23.173  < 2e-16 ***
#  c4L   -7.755161   1.219586  -6.359 2.38e-10 ***
#  b4L    0.036016   0.001490  24.174  < 2e-16 ***
#  nL     0.079803   0.002769  28.815  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Mean parameters
FrefL_15 = 1.136042
SMoptL_15 =0.125 
c4L_15 = -7.755161   
b4L_15 = 0.036016
nL_15 = 0.079803


#run model for full time series
ALL_model4_NP_15 = FrefNP_15*((All_meanGPP_15/All_GPPmax_15 +nNP_15)/1+nNP_15) *(1-c4NP_15*(SMoptNP_15-All_meanSWC5_15)^2)*exp(b4NP_15*All_meanST5_15)
#run model for full time series based on pulse time parameters
All_model4_P_15 = FrefP_15*((All_meanGPP_15/All_GPPmax_15 +nP_15)/1+nP_15) *(1-c4P_15*(SMoptP_15-All_meanSWC5_15)^2)*exp(b4P_15*All_meanST5_15)
#run all time model
All_model4_15 = FrefL_15*((All_meanGPP_15/All_GPPmax_15 +nL_15)/1+nL_15) *(1-c4L_15*(SMoptL_15-All_meanSWC5_15)^2)*exp(b4L_15*All_meanST5_15)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP_15, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P_15, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4_15, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "15_more Model", "15_less Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")



###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df15 <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df15$PulseM_15 <- All_model4_P_15
Reco_df15$NonPulseM_15 <- ALL_model4_NP_15
Reco_df15$MeanM_15 <- All_model4_15

Reco15 <- Reco_df15 %>%
  select (date, meanRECO, max_pulse_duration, PulseM_15, NonPulseM_15, MeanM_15) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM_15,
                   max_pulse_duration == 8 ~ PulseM_15,
                   max_pulse_duration == 14 ~ PulseM_15,
                   max_pulse_duration == 20 ~ PulseM_15))

Reco_Measured_15 = sum(Reco15$meanRECO, na.rm = TRUE)
Reco_PandNP_15 = sum(Reco15$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod_15 = sum(Reco15$MeanM, na.rm = TRUE)


plot(Reco15$date, Reco15$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco15$date, Reco15$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco15$date, Reco15$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "15 % model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


Reco15$Reco_Combined <- Reco15$`case_when(...)`


write.csv(Reco15, file = "data/Reco15.csv")










##### Find model parameters for each year ######################
################################################################

# NP-model - 15% SWC ##########
###############################

years_sum2_15less$year <- substr(years_sum2_15less$date, 1,4)
years_sum2_15less$year <- as.numeric(as.character(years_sum2_15less$year))

yearID1 <- unique(years_sum2_15less$year)

start1 <- list(FrefNP_15=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre1 <- data.frame(matrix(nrow = length(yearID1), ncol = 1+length(start1)))
names(params.pre1) <- c("yearID1", names(start1))

for(i in seq_along(yearID1)) {
  # create data frame for sub "i"
  
  individual_DFs1 <- years_sum2_15less %>% filter (year %in% yearID1[i])
  
  # fit model for each sub "i"
  Param_model4_NP1 <- nlsLM(meanRECO ~ FrefNP_15*((meanGPP_NP_15/GPPmax_NP_15 +n)/1+n) *
                              (1-c4*(0.1-meanSWC5_NP_15)^2)*exp(b4*meanST5_NP_15), 
                            data = individual_DFs1,
                            start = start1, #trace = TRUE,
  )
  
  # store IDs
  params.pre1[i,1] <- yearID1[i]
  
  # store fit parameters
  params.pre1[i,2:ncol(params.pre1)] <- Param_model4_NP1$m$getPars()
  
  
  
}

params.pre1


# Pulse model
years_sum2_15more$year <- substr(years_sum2_15more$date, 1,4)
years_sum2_15more$year <- as.numeric(as.character(years_sum2_15more$year))

yearID <- unique(years_sum2_15more$year)

start <- list(FrefP_15=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre <- data.frame(matrix(nrow = length(yearID), ncol = 1+length(start)))
names(params.pre) <- c("yearID", names(start))


for(i in seq_along(yearID)) {
  # create data frame for sub "i"
  
  individual_DFs <- years_sum2_15more%>% filter (year %in% yearID[i])
  
  # fit model for each sub "i"
  Param_model4_P1 <- nlsLM(meanRECO ~ FrefP_15*((meanGPP_P_15/GPPmax_P_15 +n)/1+n)* 
                             (1-c4*(0.1-meanSWC5_P_15)^2)*exp(b4*meanST5_P_15), 
                           data = individual_DFs,
                           start = start, trace = TRUE,
                           #control = nls.control(maxiter = 1000, minFactor = 0.01)
  )
  
  # store IDs
  params.pre[i,1] <- yearID[i]
  
  # store fit parameters
  params.pre[i,2:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  #params.pre[i,3:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  
  
}

params.pre

# calculate Stat for 15 MODEL
rmse_15Mod <- sqrt(sum((Reco15$Reco_Combined - Reco15$meanRECO)^2, na.rm=TRUE)/nrow(Reco15))
mape_15Mod <- mean(abs((Reco15$Reco_Combined - Reco15$meanRECO) / Reco15$meanRECO), na.rm=TRUE) * 100

# calculate R-squared
r_squared_15Mod <- cor(Reco15$Reco_Combined, Reco15$meanRECO, use = "complete.obs")^2


# calculate Stat for PULSE MODEL
rmse_P15Mod <- sqrt(sum((Reco15$PulseM_15 - Reco15$meanRECO)^2, na.rm=TRUE)/nrow(Reco15))
mape_P15Mod <- mean(abs((Reco15$PulseM_15 - Reco15$meanRECO) / Reco15$meanRECO), na.rm=TRUE) * 100
r_squared_P15Mod <- cor(Reco15$PulseM_15, Reco15$meanRECO, use = "complete.obs")^2

broom:: glance(Param_model4_P_15)

# calculate Stat for NON-PULSE MODEL
rmse_15NPMod <- sqrt(sum((Reco15$NonPulseM_15 - Reco15$meanRECO)^2, na.rm=TRUE)/nrow(Reco15))
mape_15NPMod <- mean(abs((Reco15$NonPulseM_15 - Reco15$meanRECO) / Reco15$meanRECO), na.rm=TRUE) * 100
r_squared_15NPMod <- cor(Reco15$NonPulseM_15, Reco15$meanRECO, use = "complete.obs")^2

broom:: glance(Param_model4_NP_15)


############## Cumulative fluxes ###########################
############################################################

Recodf_new15 <- Reco15 %>%
  na.omit() %>%
  select (date, meanRECO, MeanM_15, Reco_Combined)

Recodf_new15$culMeasured <- ave(Recodf_new15$meanRECO, FUN = cumsum)  
Recodf_new15$culMeanMod <- ave(Recodf_new15$MeanM_15, FUN = cumsum)  
Recodf_new15$culModelled <- ave(Recodf_new15$Reco_Combined, FUN = cumsum)  



plot(Recodf_new15$date,Recodf_new15$culMeasured,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Cumulative Reco", cex = 0.8)
lines(Recodf_new15$date, Recodf_new15$culMeanMod, type = "l", col = "red")
lines(Recodf_new15$date, Recodf_new15$culModelled, type = "l", col = "green")

legend(x = "topleft",
       legend = c("Measured Reco", "Mean model", "15% model"),
       pch = c(1, 16, 16),
       col = c("blue", "red", "green"),
       lty = c(NA, 1,1),
       bty = "n")


Recodf_new15$diffMean <- Recodf_new15$culMeasured - Recodf_new15$culMeanMod 
Recodf_new15$diffComb <- Recodf_new15$culMeasured - Recodf_new15$culModelled 


plot(Recodf_new15$date,Recodf_new15$diffComb,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-60,200)
)
lines(Recodf_new15$date, Recodf_new15$diffMean, type = "l", col = "red")

legend(x = "topleft",
       legend = c("15% model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")


Recodf_new15 %>%
  ggplot(aes(x=date))+ 
  geom_line(aes(y = diffMean, col = 'diffMean'))+
  geom_line(aes(y = diffComb, col = 'diffComb'))+
  theme_classic()+
  theme(text = element_text(size = 15))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  #stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Modelled Rsoil, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Year')


Recodf_new15$diffMeanV <- Recodf_new15$meanRECO - Recodf_new15$MeanM_15 
Recodf_new15$diffCombV <- Recodf_new15$meanRECO - Recodf_new15$Reco_Combined 


plot(Recodf_new15$date,Recodf_new15$diffCombV,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-2,3)
)
lines(Recodf_new15$date, Recodf_new15$diffMeanV, type = "l", col = "red")

legend(x = "topleft",
       legend = c( "15% model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")

write.csv(Recodf_new15, file = "data/Recodf_new15.csv")


# =====================================================
# >>> Read_chamber.R
# =====================================================

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











# =====================================================
# >>> Pulse and NP chamber.R
# =====================================================

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
            meanSWC = mean(meanVWC, na.rm = TRUE),
            max_pulse_duration = mean(max_pulse_duration)
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
            meanSWC = mean(meanVWC, na.rm = TRUE),
            max_pulse_duration = mean(max_pulse_duration)
            
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
            meanSWC = mean(meanVWC, na.rm = TRUE),
            max_pulse_duration = mean(max_pulse_duration)
            
  )

Sum_Chamber_NP$DOY <- yday(Sum_Chamber_NP$date)
Sum_Chamber_NP1 <- Sum_Chamber_NP[-(642),]

# Save these three data frames
# Create df for Pulse and Non-pulse time
write.csv(Sum_Chamber_all1, "data/All summary chamber.csv")
write.csv(Sum_Chamber_NP1, "data/NonPulse sum chamber.csv")
write.csv(Sum_Chamber_P1, "data/Pulse sum chamber.csv")







# =====================================================
# >>> GPP vs Rsoil chamber.R
# =====================================================

# 17-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(ggpubr)

# In this file we will plot figures to show interactions between Rsoil and GPP

# Open new- updated files with Dave's pulse definition
summary_Cham <- read.csv("data/All summary chamber.csv")
Pulse_Cham <- read.csv("data/Pulse sum chamber.csv")
NonPulse_Cham <- read.csv("data/NonPulse sum chamber.csv")


# Plot figure for all time
summary_Cham %>%
  ggplot(aes(x=meanGPP, y = meanRsoil))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Soil emission, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('All time')+
  ylim(0,6)

# Plot figure for Pulse time
Pulse_Cham %>%
  ggplot(aes(x=meanGPP, y = meanRsoil))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Soil emission, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Pulse time')+
  ylim(0,6)

# Plot figure for non-pulse time
NonPulse_Cham %>%
  ggplot(aes(x=meanGPP, y = meanRsoil))+
  geom_point(shape=1)+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Soil emission, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("GPP, ", mu, "mol m"^-2,"s"^-1))+
  ggtitle('Non-Pulse time')+
  ylim(0,6)+
  xlim(0,6)



# =====================================================
# >>> SWC ST space chamber.R
# =====================================================

# 27-06-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we will plot figures to show interactions between Rsoil and GPP

# Open file we need

summary_Cham <- read.csv("data/All summary chamber.csv")
Pulse_Cham <- read.csv("data/Pulse sum chamber.csv")
NonPulse_Cham <- read.csv("data/NonPulse sum chamber.csv")

Pulse_Cham$SWC_per <- Pulse_Cham$meanSWC*100

hist(Pulse_Cham$SWC_per)

# Plot the same for the mean ST / SWC
plot(Pulse_Cham$SWC_per, Pulse_Cham$meanTsoil, 
     cex = Pulse_Cham$meanRsoil/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC 5 cm, %", # add x-axis label
     ylab = "Mean Tsoil 5 cm, °C", # add y-axis label
     main = "SWC and Soil T - during pulses", # add plot title
     xlim=c(0,60))


NonPulse_Cham$SWC_per <- NonPulse_Cham$meanSWC*100

plot(NonPulse_Cham$SWC_per, NonPulse_Cham$meanTsoil, 
     cex = NonPulse_Cham$meanRsoil/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC 5 cm, %", # add x-axis label
     ylab = "Mean Tsoil 5 cm, °C", # add y-axis label
     main = "SWC and Soil T - between pulses" , # add plot title
     xlim=c(0,60))






# =====================================================
# >>> 15% for Rsoil.R
# =====================================================

###########################################################################
################## THRESHOLD MOISTURE SEARCH ##############################
###########################################################################

# 13-03-2025
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(stats)
library(grDevices)
library(readr)
library(ggpubr)
library(minpack.lm)


# Read other pulse division docs - with "DM" in their names
summary_Cham <- read.csv("data/All summary chamber.csv")
summary_Cham <- na.omit(summary_Cham)
summary_Cham$date <- as.Date(summary_Cham$date)


years_sum2 <- summary_Cham %>%
  select(c(2:7 | 9))


#### Divide df years_sum1 to two dfs based on different moisture values ########
### We choose moisture levels 15 % SWC5 



years_sum2$Threshold_15 <- years_sum2$meanSWC >= 0.15
years_sum2$Threshold_15 <- as.numeric(as.logical(years_sum2$Threshold_15))


################ Divide th whole df to two groups and make modelling procedure without pulse duration things 

############################################
# 15% Threshold #############################
############################################

years_sum2_15more <- years_sum2 %>%
  filter(Threshold_15 == 1)

years_sum2_15less <- years_sum2 %>%
  filter(Threshold_15 == 0)

# Assign variables
meanSWC5_NP_15 <- years_sum2_15less$meanSWC
meanST5_NP_15 <- years_sum2_15less$meanTsoil
meanGPP_NP_15 <- years_sum2_15less$meanGPP
GPPmax_NP_15 <- max(years_sum2_15less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP_15 <- nls(meanRsoil ~ FrefNP_15*((meanGPP_NP_15/GPPmax_NP_15 +n)/1+n) *
                            (1-c4*(0.1-meanSWC5_NP_15)^2)*exp(b4*meanST5_NP_15), 
                          data = years_sum2_15less,
                          start = list(FrefNP_15=0.75, c4=56.54, b4=0.04, n=0.84),
                          control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP_15 = summary(Param_model4_NP_15)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP_15   1.198846   0.083108  14.425   <2e-16 ***
# c4        -10.430798  14.957704  -0.697    0.486    
# b4          0.023866   0.002152  11.092   <2e-16 ***
#  n           0.101656   0.006397  15.892   <2e-16 ***

FrefNP_15 = 1.198846
SMoptNP_15 =0.125 
c4NP_15 = -10.430798
b4NP_15 =  0.023866
nNP_15=   0.101656

########## Fit Pulse model ##################

# Assign variables
meanSWC5_P_15 = years_sum2_15more$meanSWC
meanST5_P_15 = years_sum2_15more$meanTsoil
meanGPP_P_15 = years_sum2_15more$meanGPP
GPPmax_P_15 = max(years_sum2_15more$meanGPP, na.rm=TRUE)

Param_model4_P_15 <- nls(meanRsoil ~ FrefP_15*((meanGPP_P_15/GPPmax_P_15 +n)/1+n) *(1-c4*(0.1-meanSWC5_P_15)^2)*exp(b4*meanST5_P_15), 
                         data = years_sum2_15more,
                         start = list(FrefP_15=0.75, c4=56.54, b4=0.04, n=0.84),
                         control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P_15 = summary(Param_model4_P_15)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP_15  0.371242   0.046867   7.921 2.39e-14 ***
#  c4       -2.272085   0.690553  -3.290  0.00109 ** 
#  b4        0.046795   0.003131  14.947  < 2e-16 ***
#  n         0.338840   0.037098   9.134  < 2e-16 ***

# Pulse parameters
FrefP_15 =  0.371242
SMoptP_15 =0.125 
c4P_15 = -2.272085   
b4P_15 =  0.046795
nP_15 = 0.338840


########## Fit All-time model ##################

# Setting up drivers for all time

All_meanSWC5 = years_sum2$meanSWC
All_meanST5 = years_sum2$meanTsoil
All_meanGPP = years_sum2$meanGPP
All_GPPmax = max(years_sum2$meanGPP, na.rm = TRUE)

Param_model4_All <- nls(meanRsoil ~ FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(0.1-All_meanSWC5)^2)*exp(b4L*All_meanST5), 
                        data = years_sum2,
                        start = list(FrefL=0.75,  c4L=56.54, b4L=0.04, nL=0.84),
                        control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_All = summary(Param_model4_All)

#Parameters:
#  Estimate Std. Error t value Pr(>|t|)    
#FrefL  0.916427   0.062004  14.780   <2e-16 ***
#  c4L   -0.244660   0.542270  -0.451    0.652    
#b4L    0.031851   0.001996  15.960   <2e-16 ***
#  nL     0.137391   0.007402  18.561   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Mean parameters
FrefL = 0.916427
SMoptL =0.125 
c4L = -0.244660  
b4L = 0.031851
nL= 0.137391


#run model for full time series
ALL_model4_NP_15 = FrefNP_15*((All_meanGPP/All_GPPmax +nNP_15)/1+nNP_15) *(1-c4NP_15*(SMoptNP_15-All_meanSWC5)^2)*exp(b4NP_15*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P_15 = FrefP_15*((All_meanGPP/All_GPPmax +nP_15)/1+nP_15) *(1-c4P_15*(SMoptP_15-All_meanSWC5)^2)*exp(b4P_15*All_meanST5)
#run all time model
All_model4_15 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRsoil, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Rsoil", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP_15, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P_15, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4_15, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "15_more Model", "15_less Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")



###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Rsoil_df15 <- summary_Cham %>%
    select(date, meanRsoil, max_pulse_duration)

Rsoil_df15$PulseM_15 <- All_model4_P_15
Rsoil_df15$NonPulseM_15 <- ALL_model4_NP_15
Rsoil_df15$MeanM_15 <- All_model4_15

Rsoil15 <- Rsoil_df15 %>%
  select (date, meanRsoil, max_pulse_duration, PulseM_15, NonPulseM_15, MeanM_15) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM_15,
                   max_pulse_duration == 8 ~ PulseM_15,
                   max_pulse_duration == 14 ~ PulseM_15,
                   max_pulse_duration == 20 ~ PulseM_15))

Rsoil_Measured_15 = sum(Rsoil15$meanRsoil, na.rm = TRUE)
Rsoil_PandNP_15 = sum(Rsoil15$`case_when(...)`, na.rm = TRUE)
Rsoil_MeanMod_15 = sum(Rsoil15$MeanM_15, na.rm = TRUE)


plot(Rsoil15$date, Rsoil15$meanRsoil, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Rsoil15$date, Rsoil15$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Rsoil15$date, Rsoil15$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Rsoil", "15 % model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


Rsoil15$Rsoil_Combined <- Rsoil15$`case_when(...)`


##### Find model parameters for each year ######################
################################################################

# NP-model - 15% SWC ##########
###############################

years_sum2_15less$year <- substr(years_sum2_15less$date, 1,4)
years_sum2_15less$year <- as.numeric(as.character(years_sum2_15less$year))

yearID1 <- unique(years_sum2_15less$year)

start1 <- list(FrefNP_15=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre1 <- data.frame(matrix(nrow = length(yearID1), ncol = 1+length(start1)))
names(params.pre1) <- c("yearID1", names(start1))

for(i in seq_along(yearID1)) {
  # create data frame for sub "i"
  
  individual_DFs1 <- years_sum2_15less %>% filter (year %in% yearID1[i])
  
  # fit model for each sub "i"
  Param_model4_NP1 <- nlsLM(meanRsoil ~ FrefNP_15*((meanGPP_NP_15/GPPmax_NP_15 +n)/1+n) *
                              (1-c4*(0.1-meanSWC5_NP_15)^2)*exp(b4*meanST5_NP_15), 
                            data = individual_DFs1,
                            start = start1, #trace = TRUE,
  )
  
  # store IDs
  params.pre1[i,1] <- yearID1[i]
  
  # store fit parameters
  params.pre1[i,2:ncol(params.pre1)] <- Param_model4_NP1$m$getPars()
  
  
  
}

params.pre1


# Pulse model
years_sum2_15more$year <- substr(years_sum2_15more$date, 1,4)
years_sum2_15more$year <- as.numeric(as.character(years_sum2_15more$year))

yearID <- unique(years_sum2_15more$year)

start <- list(FrefP_15=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre <- data.frame(matrix(nrow = length(yearID), ncol = 1+length(start)))
names(params.pre) <- c("yearID", names(start))


for(i in seq_along(yearID)) {
  # create data frame for sub "i"
  
  individual_DFs <- years_sum2_15more%>% filter (year %in% yearID[i])
  
  # fit model for each sub "i"
  Param_model4_P1 <- nlsLM(meanRsoil ~ FrefP_15*((meanGPP_P_15/GPPmax_P_15 +n)/1+n) *(1-c4*(0.1-meanSWC5_P_15)^2)*exp(b4*meanST5_P_15), 
                           data = individual_DFs,
                           start = start, trace = TRUE,
                           #control = nls.control(maxiter = 1000, minFactor = 0.01)
  )
  
  # store IDs
  params.pre[i,1] <- yearID[i]
  
  # store fit parameters
  params.pre[i,2:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  #params.pre[i,3:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  
  
}

params.pre

# calculate Stat for 15 MODEL
rmse_15Mod <- sqrt(sum((Rsoil15$Rsoil_Combined - Rsoil15$meanRsoil)^2, na.rm=TRUE)/nrow(Rsoil15))
mape_15Mod <- mean(abs((Rsoil15$Rsoil_Combined - Rsoil15$meanRsoil) / Rsoil15$meanRsoil), na.rm=TRUE) * 100
r_squared_15Mod <- cor(Rsoil15$Rsoil_Combined, Rsoil15$meanRsoil, use = "complete.obs")^2


# calculate Stat for PULSE MODEL
rmse_P15Mod <- sqrt(sum((Rsoil15$PulseM_15 - Rsoil15$meanRsoil)^2, na.rm=TRUE)/nrow(Rsoil15))
mape_P15Mod <- mean(abs((Rsoil15$PulseM_15 - Rsoil15$meanRsoil) / Rsoil15$meanRsoil), na.rm=TRUE) * 100
r_squared_P15Mod <- cor(Rsoil15$PulseM_15, Rsoil15$meanRsoil, use = "complete.obs")^2

broom:: glance(Param_model4_P_15)

# calculate Stat for NON-PULSE MODEL
rmse_15NPMod <- sqrt(sum((Rsoil15$NonPulseM_15 - Rsoil15$meanRsoil)^2, na.rm=TRUE)/nrow(Rsoil15))
mape_15NPMod <- mean(abs((Rsoil15$NonPulseM_15 - Rsoil15$meanRsoil) / Rsoil15$meanRsoil), na.rm=TRUE) * 100
r_squared_15NPMod <- cor(Rsoil15$NonPulseM_15, Rsoil15$meanRsoil, use = "complete.obs")^2

broom:: glance(Param_model4_NP_15)


############## Cumulative fluxes ###########################
############################################################

Rsoildf_new15 <- Rsoil15 %>%
  na.omit() %>%
  select (date, meanRsoil, MeanM_15, Rsoil_Combined)

Rsoildf_new15$culMeasured <- ave(Rsoildf_new15$meanRsoil, FUN = cumsum)  
Rsoildf_new15$culMeanMod <- ave(Rsoildf_new15$MeanM_15, FUN = cumsum)  
Rsoildf_new15$culModelled <- ave(Rsoildf_new15$Rsoil_Combined, FUN = cumsum)  



plot(Rsoildf_new15$date,Rsoildf_new15$culMeasured,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Cumulative Rsoil", cex = 0.8)
lines(Rsoildf_new15$date, Rsoildf_new15$culMeanMod, type = "l", col = "red")
lines(Rsoildf_new15$date, Rsoildf_new15$culModelled, type = "l", col = "green")

legend(x = "topleft",
       legend = c("Measured Rsoil", "Mean model", "15% model"),
       pch = c(1, 16, 16),
       col = c("blue", "red", "green"),
       lty = c(NA, 1,1),
       bty = "n")


Rsoildf_new15$diffMean <- Rsoildf_new15$culMeasured - Rsoildf_new15$culMeanMod
Rsoildf_new15$diffComb <- Rsoildf_new15$culMeasured - Rsoildf_new15$culModelled


plot(Rsoildf_new15$date,Rsoildf_new15$diffComb,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-60,200)
)
lines(Rsoildf_new15$date, Rsoildf_new15$diffMean, type = "l", col = "red")

legend(x = "topleft",
       legend = c("15% model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")


Rsoildf_new15 %>%
  ggplot(aes(x=date))+ 
  geom_line(aes(y = diffMean, col = 'diffMean'))+
  geom_line(aes(y = diffComb, col = 'diffComb'))+
  theme_classic()+
  theme(text = element_text(size = 15))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  #stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Modelled Rsoil, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Year')


Rsoildf_new15$diffMeanV <- Rsoildf_new15$meanRsoil - Rsoildf_new15$MeanM_15
Rsoildf_new15$diffCombV <- Rsoildf_new15$meanRsoil - Rsoildf_new15$Rsoil_Combined


plot(Rsoildf_new15$date,Rsoildf_new15$diffCombV,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-3,6)
)
lines(Rsoildf_new15$date, Rsoildf_new15$diffMeanV, type = "l", col = "red")

legend(x = "topleft",
       legend = c( "15% model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")





# =====================================================
# >>> Chamber model.R
# =====================================================

# 18-05-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(stats)
library(grDevices)
library(readr)
library(minpack.lm)
library(ggpubr)

# Open new files 
summary_Cham <- read.csv("data/All summary chamber.csv")
Pulse_Cham <- read.csv("data/Pulse sum chamber.csv")
NonPulse_Cham <- read.csv("data/NonPulse sum chamber.csv")

# Make sure in the Date format
NonPulse_Cham$date <- as.Date(NonPulse_Cham$date)
Pulse_Cham$date <- as.Date(Pulse_Cham$date)
summary_Cham$date <- as.Date(summary_Cham$date)

# CHECK COLUMN NAMES AND DATA TYPES
str(NonPulse_Cham)
str(Pulse_Cham)
str(summary_Cham)

###### Pulse time model ###########################
###################################################

# Assign variables
Pulse_Cham <- na.omit(Pulse_Cham)

meanSWC_P =Pulse_Cham$meanSWC
meanST_P = Pulse_Cham$meanTsoil
meanGPP_P = Pulse_Cham$meanGPP
GPPmax_P = max(Pulse_Cham$meanGPP, na.rm=TRUE)

# Fit model
Param_model4_P <- nls(meanRsoil ~ Fref*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC_P)^2)*exp(b4*meanST_P), 
                      data = Pulse_Cham,
                      start = list(Fref=0.75,  c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Pulse parameters 
#Parameters:
        # Estimate Std. Error t value Pr(>|t|)    
# Fref  0.446910   0.060229   7.420 7.49e-13 ***
# c4    -1.058658   0.764587  -1.385    0.167    
# b4     0.044799   0.003433  13.049  < 2e-16 ***
#  n     0.298964   0.035638   8.389 9.29e-16 ***

FrefP = 0.446910 
SMoptP =0.125 
c4P = -1.058658   
b4P = 0.044799
nP= 0.298964

####### Non-Pulse model ###################
###########################################

# Assign variables
NonPulse_Cham <- na.omit(NonPulse_Cham)

meanSWC_NP <- NonPulse_Cham$meanSWC
meanST_NP <- NonPulse_Cham$meanTsoil
meanGPP_NP <- NonPulse_Cham$meanGPP
GPPmax_NP <- max(NonPulse_Cham$meanGPP)

# Fit model
Param_model4_NP <- nls(meanRsoil ~ Fref*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC_NP)^2)*exp(b4*meanST_NP), 
                       data = NonPulse_Cham,
                       start = list(Fref=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

#Parameters:
#        Estimate Std. Error t value Pr(>|t|)    
# Fref   0.743454   0.035530  20.924   <2e-16 ***
#  c4   -1.668911   0.845241  -1.974   0.0488 *  
#  b4    0.023544   0.001437  16.383   <2e-16 ***
#  n     0.160941   0.007160  22.479   <2e-16 ***


# NON_Pulse parameters
FrefNP = 0.743454
SMoptNP =0.125 
c4NP = -1.668911  
b4NP = 0.023544
nNP=  0.160941

############### All time model #################################
################################################################

complete.cases(summary_Cham)

# Setting up drivers for all time

summary_Cham <- na.omit (summary_Cham)

All_meanSWC = summary_Cham$meanSWC
All_meanST = summary_Cham$meanTsoil
All_meanGPP = summary_Cham$meanGPP
All_GPPmax = max(summary_Cham$meanGPP, na.rm = TRUE)

Param_model4_All <- nls(meanRsoil ~ Fref*((All_meanGPP/All_GPPmax +n)/1+n) *(1-c4*(0.1-All_meanSWC)^2)*exp(b4*All_meanST), 
                        data = summary_Cham,
                        start = list(Fref=0.75,  c4=56.54, b4=0.04, n=0.84),
                        control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_All = summary(Param_model4_All)

#Parameters:
#       Estimate Std. Error t value Pr(>|t|)    
# Fref  0.916427   0.062004  14.780   <2e-16 ***
# c4   -0.244660   0.542270  -0.451    0.652    
# b4    0.031851   0.001996  15.960   <2e-16 ***
# n     0.137391   0.007402  18.561   <2e-16 ***


# All-time parameters
FrefAll = 0.916427
SMoptAll =0.125 
c4All = -0.244660   
b4All = 0.031851
nAll= 0.137391



#run model for full time series based on non-pulse time parameters
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC)^2)*exp(b4NP*All_meanST)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC)^2)*exp(b4P*All_meanST)
#run all time model
All_model4 = FrefAll*((All_meanGPP/All_GPPmax +nAll)/1+nAll) *(1-c4All*(SMoptAll-All_meanSWC)^2)*exp(b4All*All_meanST)

# Plot the RECO time series
plot(summary_Cham$date, summary_Cham$meanRsoil, type = "p", col = "blue", xlab = "Timestamp", ylab = "Rsoil", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(summary_Cham$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(summary_Cham$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(summary_Cham$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured Rsoil", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


plot(ALL_model4_NP,All_model4_P, xlab = "Non-Pulse Model Rsoil", ylab = "Pulse Model Rsoil")

# calculate RMSE
rmse_NP <- sqrt(mean((ALL_model4_NP - summary_Cham$meanRsoil)^2, na.rm = TRUE))
rmse_P <- sqrt(mean((All_model4_P - summary_Cham$meanRsoil)^2, na.rm = TRUE))
# calculate MAPE
mape_NP <- mean(abs(ALL_model4_NP - summary_Cham$meanRsoil) / summary_Cham$meanRsoil, na.rm = TRUE) * 100
mape_P <- mean(abs(All_model4_P - summary_Cham$meanRsoil) / summary_Cham$meanRsoil, na.rm = TRUE) * 100
# calculate R-squared
r_squared_NP <- cor(ALL_model4_NP, summary_Cham$meanRsoil, use = "complete.obs")^2
r_squared_P <- cor(All_model4_P, summary_Cham$meanRsoil, use = "complete.obs")^2


# Create df with all measured and modelled fluxes
Rsoil_df <- summary_Cham %>%
  select(date, meanRsoil, max_pulse_duration, )

Rsoil_df$PulseM <- All_model4_P
Rsoil_df$NonPulseM <- ALL_model4_NP
Rsoil_df$MeanM <- All_model4

Rsoil1 <- Rsoil_df %>%
  select (date, meanRsoil, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Rsoil1$threshold15 <- Rsoil15$Rsoil_Combined

plot(Rsoil1$date, Rsoil1$meanRsoil, type = "p", col = "blue", xlab = "Year", 
     ylab =  "Rsoil, µmol m-2 s-1", cex = 0.8,
     ylim = c(0,4))

points(Rsoil1$date, Rsoil1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Rsoil1$date, Rsoil1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
  points(Rsoil1$date, Rsoil1$threshold15, col="cyan", pch = 16, cex = 0.4, alpha=0.5)


# create the legend
legend(x = "topleft",
       legend = c("Measured Rsoil", "P-NP model", "Mean model", "15% model"),
       pch = c(1, 16),
       col = c("blue", "green", "red", "cyan"),
       lty = c(NA, 1),
       bty = "n")

Rsoil_df$Rsoil_Combined <- Rsoil1$`case_when(...)`

Rsoil_df %>%
  na.omit() %>%
  ggplot(aes(x=date))+
  geom_point(aes(y = meanRsoil),shape=20, color = "blue", size = 2)+
  geom_point(aes(y=Rsoil_Combined),shape=1, size = 1, color = "green")+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Rsoil, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Timestamp")+
  ylim(0,4)


Rsoil_df %>%
  ggplot(aes(x=meanRsoil, y = Rsoil_Combined))+
  geom_point(shape=1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Modelled Rsoil, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("Measured Rsoil, ", mu, "mol m"^-2,"s"^-1))+
  #ggtitle('Non-pulse time')+
  ylim(0,4)+
  xlim(0,4)

# statistics for Combined model
rmse_CombMod <- sqrt(sum((Rsoil_df$Rsoil_Combined - Rsoil_df$meanRsoil)^2, na.rm=TRUE)/nrow(Rsoil_df))

# calculate MAPE -  Mean absolute percent error
mape_CombMod <- mean(abs((Rsoil_df$Rsoil_Combined - Rsoil_df$meanRsoil) / Rsoil_df$meanRsoil), na.rm=TRUE) * 100

# calculate R-squared
r_squared_CombMod <- cor(Rsoil_df$Rsoil_Combined, Rsoil_df$meanRsoil, use = "complete.obs")^2

### statistics for Mean model
rmse_MeanMod <- sqrt(sum((Rsoil_df$MeanM - Rsoil_df$meanRsoil)^2, na.rm=TRUE)/nrow(Rsoil_df))

# calculate MAPE -  Mean absolute percent error
mape_MeanMod <- mean(abs((Rsoil_df$MeanM - Rsoil_df$meanRsoil) / Rsoil_df$meanRsoil), na.rm=TRUE) * 100

# calculate R-squared
r_squared_MeanMod <- cor(Rsoil_df$MeanM, Rsoil_df$meanRsoil, use = "complete.obs")^2



###### Calculate cumulative flux for - Mean model, Including Pulse and non-pulse together and measured fluxes ######
####################################################################################################################

### Create df with just fluxes #####

Rsoildf_new <- Rsoil_df %>%
  na.omit() %>%
  select (date, meanRsoil, MeanM, Rsoil_Combined)

Rsoildf_new$culMeasured <- ave(Rsoildf_new$meanRsoil, FUN = cumsum)  
Rsoildf_new$culMeanMod <- ave(Rsoildf_new$MeanM, FUN = cumsum)  
Rsoildf_new$culCombined <- ave(Rsoildf_new$Rsoil_Combined, FUN = cumsum)  

Rsoildf_new$cumRsoil15 <- Rsoildf_new15$culModelled

plot(Rsoildf_new$date,Rsoildf_new$culMeasured,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Cumulative Rsoil", cex = 0.8)
lines(Rsoildf_new$date, Rsoildf_new$culMeanMod, type = "l", col = "red")
lines(Rsoildf_new$date, Rsoildf_new$culCombined, type = "l", col = "green")
lines(Rsoildf_new$date, Rsoildf_new$cumRsoil15, type = "l", col = "cyan")

legend(x = "topleft",
       legend = c("Measured Rsoil", "Mean model", "P-NP model", "15% model" ),
       pch = c(1, 16, 16, 16),
       col = c("blue", "red", "green", "cyan"),
       lty = c(NA, 1,1),
       bty = "n")



sum(Rsoildf_new$meanRsoil)
sum(Rsoildf_new$MeanM)
sum(Rsoildf_new$Rsoil_Combined)

######### Difference Rsoil Measured - Modelled 
Rsoildf_new$diffMean <- Rsoildf_new$culMeasured - Rsoildf_new$culMeanMod
Rsoildf_new$diffComb <- Rsoildf_new$culMeasured - Rsoildf_new$culCombined

Rsoildf_new$diff15M <- Rsoildf_new15$diffComb



plot(Rsoildf_new$date,Rsoildf_new$diffComb,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-60,80)
     )
lines(Rsoildf_new$date, Rsoildf_new$diffMean, type = "l", col = "red")
lines(Rsoildf_new$date, Rsoildf_new$diff15M, type = "l", col = "cyan")


legend(x = "topleft",
       legend = c("P-NP model difference", "Mean model difference", "15% model difference"),
       pch = c(1, 16, 16, 16),
       col = c("blue", "red", "cyan"),
       lty = c(NA, 1, 1, 1),
       bty = "n")


Rsoildf_new %>%
  ggplot(aes(x=date))+ 
  geom_line(aes(y = diffMean, col = 'diffMean'))+
  geom_line(aes(y = diffComb, col = 'diffComb'))+
  theme_classic()+
  theme(text = element_text(size = 15))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  #stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Modelled Rsoil, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Year')
  #ggtitle('Non-pulse time')+
  #ylim(0,4)+
  #xlim(0,4)


Rsoildf_new$diffMeanV <- Rsoildf_new$meanRsoil - Rsoildf_new$MeanM
Rsoildf_new$diffCombV <- Rsoildf_new$meanRsoil - Rsoildf_new$Rsoil_Combined

Rsoildf_new$diff15Meas <- Rsoildf_new15$diffCombV

plot(Rsoildf_new$date,Rsoildf_new$diffCombV,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-3,6)
)
lines(Rsoildf_new$date, Rsoildf_new$diffMeanV, type = "l", col = "red")
lines(Rsoildf_new$date, Rsoildf_new$diff15Meas, type = "l", col = "cyan")

legend(x = "topleft",
       legend = c( "P-NP model difference", "Mean model difference", "15% model difference"),
       pch = c(1, 16, 16, 16),
       col = c("blue", "red", "cyan"),
       lty = c(NA, 1, 1, 1),
       bty = "n")


##### Find model parameters for each year ######################
################################################################

# NP-model
NonPulse_Cham$year <- substr(NonPulse_Cham$date, 1,4)
NonPulse_Cham$year <- as.numeric(as.character(NonPulse_Cham$year))

yearID1 <- unique(NonPulse_Cham$year)

start1 <- list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre1 <- data.frame(matrix(nrow = length(yearID1), ncol = 1+length(start1)))
names(params.pre1) <- c("yearID1", names(start1))

for(i in seq_along(yearID1)) {
  # create data frame for sub "i"
  
  individual_DFs1 <- NonPulse_Cham %>% filter (year %in% yearID1[i])
  
  # fit model for each sub "i"
  Param_model4_NP1 <- nlsLM(meanRsoil ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n)*
                              (1-c4*(0.1-meanSWC_NP)^2)*exp(b4*meanST_NP), 
                            data = individual_DFs1,
                            start = start1, trace = TRUE
  )
  
  # store IDs
  params.pre1[i,1] <- yearID1[i]
  
  # store fit parameters
  params.pre1[i,2:ncol(params.pre1)] <- Param_model4_NP1$m$getPars()
  
 
  
}

params.pre1


# Pulse model
Pulse_Cham$year <- substr(Pulse_Cham$date, 1,4)
Pulse_Cham$year <- as.numeric(as.character(Pulse_Cham$year))

yearID <- unique(Pulse_Cham$year)

start <- list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre <- data.frame(matrix(nrow = length(yearID), ncol = 1+length(start)))
names(params.pre) <- c("yearID", names(start))


for(i in seq_along(yearID)) {
  # create data frame for sub "i"
  
  individual_DFs <- Pulse_Cham %>% filter (year %in% yearID[i])
  
  # fit model for each sub "i"
  Param_model4_P1 <- nlsLM(meanRsoil ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC_P)^2)*exp(b4*meanST_P), 
                           data = individual_DFs,
                           start = start, trace = TRUE,
                           #control = nls.control(maxiter = 1000, minFactor = 0.01)
  )
  
  # store IDs
  params.pre[i,1] <- yearID[i]
  
  # store fit parameters
  params.pre[i,2:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  #params.pre[i,3:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  
  
}

params.pre


# Mean Model 
summary_Cham$year <- substr(summary_Cham$date, 1,4)
summary_Cham$year <- as.numeric(as.character(summary_Cham$year))

yearID2 <- unique(summary_Cham$year)

start2 <- list(FrefL=0.75,  c4L=56.54, b4L=0.04, nL=0.84)

# create empty data.frame to store IDs and parameters
params.pre2 <- data.frame(matrix(nrow = length(yearID2), ncol = 1+length(start2)))
names(params.pre2) <- c("yearID", names(start2))


for(i in seq_along(yearID2)) {
  # create data frame for sub "i"
  
  individual_DFs2 <- summary_Cham %>% filter (year %in% yearID2[i])
  
  # fit model for each sub "i"
  Param_model4_All1 <- nlsLM(meanRsoil ~ FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(0.1-All_meanSWC)^2)*exp(b4L*All_meanST), 
                             data = individual_DFs2,
                             start = start2, trace = TRUE,
                             #control = nls.control(maxiter = 1000, minFactor = 0.01)
  )
  
  # store IDs
  params.pre2[i,1] <- yearID2[i]
  
  # store fit parameters
  params.pre2[i,2:ncol(params.pre2)] <- Param_model4_All1$m$getPars()
  
  #params.pre[i,3:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  
  
}

params.pre2


######### AIC calculation ################

#1 NM model

AIC_1 <- AIC(Param_model4_NP)

# Print the result
cat("Automated AIC:", AIC_1, "\n")

#2 P model

AIC_2 <- AIC(Param_model4_P)

# Print the result
cat("Automated AIC:", AIC_2, "\n")


#3 All model
AIC_3 <- AIC(Param_model4_All)

# Print the result
cat("Automated AIC:", AIC_3, "\n")










