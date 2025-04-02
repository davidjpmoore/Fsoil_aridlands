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
years_sum1 <- read.csv("data/years_sum1_DM.csv")
years_sum1 <- na.omit(years_sum1)
years_sum1$date <- as.Date(years_sum1$date)

plot(years_sum1$meanSWC5, years_sum1$meanRECO)

#### Divide df years_sum1 to two dfs based on different moisture values ########
### We choose moisture levels from 5 to 15 % for SWC5 

years_sum1$Threshold_5 <- years_sum1$meanSWC5 >=5
years_sum1$Threshold_5 <- as.numeric(as.logical(years_sum1$Threshold_5))

years_sum1$Threshold_6 <- years_sum1$meanSWC5 >=6
years_sum1$Threshold_6 <- as.numeric(as.logical(years_sum1$Threshold_6))

years_sum1$Threshold_7 <- years_sum1$meanSWC5 >=7
years_sum1$Threshold_7 <- as.numeric(as.logical(years_sum1$Threshold_7))

years_sum1$Threshold_8 <- years_sum1$meanSWC5 >=8
years_sum1$Threshold_8 <- as.numeric(as.logical(years_sum1$Threshold_8))

years_sum1$Threshold_9 <- years_sum1$meanSWC5 >=9
years_sum1$Threshold_9 <- as.numeric(as.logical(years_sum1$Threshold_9))

years_sum1$Threshold_10 <- years_sum1$meanSWC5 >=10
years_sum1$Threshold_10 <- as.numeric(as.logical(years_sum1$Threshold_10))

years_sum1$Threshold_11 <- years_sum1$meanSWC5 >=11
years_sum1$Threshold_11 <- as.numeric(as.logical(years_sum1$Threshold_11))

years_sum1$Threshold_12 <- years_sum1$meanSWC5 >=12
years_sum1$Threshold_12 <- as.numeric(as.logical(years_sum1$Threshold_12))

years_sum1$Threshold_13 <- years_sum1$meanSWC5 >=13
years_sum1$Threshold_13 <- as.numeric(as.logical(years_sum1$Threshold_13))

years_sum1$Threshold_14 <- years_sum1$meanSWC5 >=14
years_sum1$Threshold_14 <- as.numeric(as.logical(years_sum1$Threshold_14))

years_sum1$Threshold_15 <- years_sum1$meanSWC5 >=15
years_sum1$Threshold_15 <- as.numeric(as.logical(years_sum1$Threshold_15))


################ Divide th whole df to two groups and make modelling procedure without pulse duration things 

years_sum2 <- years_sum1 %>%
  select(c(1:19 | 26:36))

# 5% Threshold #############################
years_sum2_5more <- years_sum2 %>%
  filter(Threshold_5 == 1)

years_sum2_5less <- years_sum2 %>%
  filter(Threshold_5 == 0)

# Assign variables
SoilMoisture <- years_sum2_5less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_5less$meanST5
meanGPP_NP <- years_sum2_5less$meanGPP
GPPmax_NP <- max(years_sum2_5less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_5less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# Formula: meanRECO ~ Fref * ((meanGPP_NP/GPPmax_NP + n)/1 + n) * 
# (1 - c4 * (0.1 - meanSWC5_NP)^2) * exp(b4 * meanST5_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.539808   0.080667   6.692 7.20e-11 ***
#  c4     52.007396   9.220671   5.640 3.15e-08 ***
#  b4      0.035296   0.004538   7.779 5.93e-14 ***
#  n       0.176950   0.014268  12.402  < 2e-16 ***


FrefNP = 0.539808
SMoptNP =0.125 
c4NP = 52.007396
b4NP =  0.035296
nNP=  0.176950

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_5more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_5more$meanST5
meanGPP_P = years_sum2_5more$meanGPP
GPPmax_P = max(years_sum2_5more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_5more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.883180   0.044295  19.939   <2e-16 ***
#  c4    -10.925318   1.313771  -8.316   <2e-16 ***
#  b4      0.042876   0.001646  26.048   <2e-16 ***
#  n       0.101513   0.004101  24.753   <2e-16 ***

# Pulse parameters
FrefP = 0.883180
SMoptP =0.125 
c4P = -10.925318   
b4P = 0.042876
nP= 0.101513

########## Fit All-time model ##################

# Setting up drivers for all time
SoilMoisture_all =years_sum1$meanSWC5/100
All_meanSWC5 = SoilMoisture_all
All_meanST5 = years_sum1$meanST5
All_meanGPP = years_sum1$meanGPP
All_GPPmax = max(years_sum1$meanGPP, na.rm = TRUE)

Param_model4_All <- nls(meanRECO ~ FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(0.1-All_meanSWC5)^2)*exp(b4L*All_meanST5), 
                        data = years_sum1,
                        start = list(FrefL=0.75,  c4L=56.54, b4L=0.04, nL=0.84),
                        control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_All = summary(Param_model4_All)


#Parameters:
#  Estimate Std. Error t value Pr(>|t|)    
#FrefL  1.136042   0.049023  23.173  < 2e-16 ***
#  c4L   -7.755161   1.219586  -6.359 2.38e-10 ***
#  b4L    0.036016   0.001490  24.174  < 2e-16 ***
#  nL     0.079803   0.002769  28.815  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Mean parameters
FrefL = 1.136042
SMoptL =0.125 
c4L = -7.755161   
b4L = 0.036016
nL= 0.079803

#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")
       

###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")

Reco_df$Reco_Combined <- Reco1$`case_when(...)`

# 6% Threshold #############################
years_sum2_6more <- years_sum2 %>%
  filter(Threshold_6 == 1)

years_sum2_6less <- years_sum2 %>%
  filter(Threshold_6 == 0)

# Assign variables
SoilMoisture <- years_sum2_6less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_6less$meanST5
meanGPP_NP <- years_sum2_6less$meanGPP
GPPmax_NP <- max(years_sum2_6less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_6less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.341350   0.037979   8.988  < 2e-16 ***
#  c4     47.796114   7.500974   6.372 3.68e-10 ***
#  b4      0.051673   0.003741  13.813  < 2e-16 ***
#  n       0.162858   0.011423  14.257  < 2e-16 ***


FrefNP = 0.341350
SMoptNP =0.125 
c4NP = 47.796114
b4NP =  0.051673
nNP=  0.162858

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_6more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_6more$meanST5
meanGPP_P = years_sum2_6more$meanGPP
GPPmax_P = max(years_sum2_6more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_6more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.807527   0.043974  18.364   <2e-16 ***
#  c4    -11.827586   1.347246  -8.779   <2e-16 ***
#  b4      0.044752   0.001739  25.731   <2e-16 ***
#  n       0.115189   0.005042  22.847   <2e-16 ***

# Pulse parameters
FrefP = 0.807527
SMoptP =0.125 
c4P = -11.827586   
b4P = 0.044752
nP= 0.115189

#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")

Reco_df$Reco_Combined <- Reco1$`case_when(...)`

############################################
# 7% Threshold #############################
############################################

years_sum2_7more <- years_sum2 %>%
  filter(Threshold_7 == 1)

years_sum2_7less <- years_sum2 %>%
  filter(Threshold_7 == 0)

# Assign variables
SoilMoisture <- years_sum2_7less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_7less$meanST5
meanGPP_NP <- years_sum2_7less$meanGPP
GPPmax_NP <- max(years_sum2_7less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_7less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.304624   0.027252  11.178  < 2e-16 ***
#  c4     44.325508   6.751482   6.565 9.85e-11 ***
#  b4      0.053561   0.003114  17.198  < 2e-16 ***
#  n       0.174975   0.011172  15.662  < 2e-16 ***


FrefNP = 0.304624
SMoptNP =0.125 
c4NP = 44.325508
b4NP =  0.053561
nNP=  0.174975

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_7more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_7more$meanST5
meanGPP_P = years_sum2_7more$meanGPP
GPPmax_P = max(years_sum2_7more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_7more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.792617   0.045209   17.53   <2e-16 ***
# c4    -11.729643   1.365504   -8.59   <2e-16 ***
# b4      0.045089   0.001806   24.96   <2e-16 ***
# n       0.120244   0.005506   21.84   <2e-16 ***
  
# Pulse parameters
FrefP = 0.792617
SMoptP =0.125 
c4P = -11.729643   
b4P = 0.045089
nP= 0.120244


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")

############################################
# 8% Threshold #############################
############################################

years_sum2_8more <- years_sum2 %>%
  filter(Threshold_8 == 1)

years_sum2_8less <- years_sum2 %>%
  filter(Threshold_8 == 0)

# Assign variables
SoilMoisture <- years_sum2_8less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_8less$meanST5
meanGPP_NP <- years_sum2_8less$meanGPP
GPPmax_NP <- max(years_sum2_8less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_8less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.316117   0.024806  12.743  < 2e-16 ***
#  c4     44.485023   6.228604   7.142 2.02e-12 ***
#  b4      0.053221   0.002778  19.160  < 2e-16 ***
#  n       0.177341   0.010807  16.409  < 2e-16 ***


FrefNP = 0.316117
SMoptNP =0.125 
c4NP = 44.485023
b4NP =  0.053221 
nNP=   0.177341

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_8more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_8more$meanST5
meanGPP_P = years_sum2_8more$meanGPP
GPPmax_P = max(years_sum2_8more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_8more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.792617   0.045209   17.53   <2e-16 ***
#  c4    -11.729643   1.365504   -8.59   <2e-16 ***
#  b4      0.045089   0.001806   24.96   <2e-16 ***
#  n       0.120244   0.005506   21.84   <2e-16 ***

# Pulse parameters
FrefP = 0.792617
SMoptP =0.125 
c4P = -11.729643   
b4P = 0.045089
nP= 0.120244


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")

############################################
# 9% Threshold #############################
############################################

years_sum2_9more <- years_sum2 %>%
  filter(Threshold_9 == 1)

years_sum2_9less <- years_sum2 %>%
  filter(Threshold_9 == 0)

# Assign variables
SoilMoisture <- years_sum2_9less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_9less$meanST5
meanGPP_NP <- years_sum2_9less$meanGPP
GPPmax_NP <- max(years_sum2_9less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_9less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.441130   0.029305  15.053   <2e-16 ***
#  c4     48.309900   5.734171   8.425   <2e-16 ***
#  b4      0.048899   0.002361  20.708   <2e-16 ***
#  n       0.151720   0.008722  17.395   <2e-16 ***


FrefNP = 0.441130
SMoptNP =0.125 
c4NP = 48.309900
b4NP =  0.048899
nNP=   0.151720

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_9more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_9more$meanST5
meanGPP_P = years_sum2_9more$meanGPP
GPPmax_P = max(years_sum2_9more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_9more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.840661   0.051568   16.30  < 2e-16 ***
#  c4    -10.537187   1.403140   -7.51 9.43e-14 ***
#  b4      0.043637   0.001962   22.25  < 2e-16 ***
#  n       0.116878   0.005682   20.57  < 2e-16 ***

# Pulse parameters
FrefP = 0.840661
SMoptP =0.125 
c4P = -10.537187   
b4P = 0.043637
nP= 0.116878


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


############################################
# 10% Threshold #############################
############################################

years_sum2_10more <- years_sum2 %>%
  filter(Threshold_10 == 1)

years_sum2_10less <- years_sum2 %>%
  filter(Threshold_10 == 0)

# Assign variables
SoilMoisture <- years_sum2_10less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_10less$meanST5
meanGPP_NP <- years_sum2_10less$meanGPP
GPPmax_NP <- max(years_sum2_10less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_10less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.503082   0.028022  17.953  < 2e-16 ***
#  c4     37.253799   5.243011   7.105 1.98e-12 ***
#  b4      0.047130   0.002011  23.442  < 2e-16 ***
#  n       0.120450   0.005637  21.370  < 2e-16 ***

FrefNP = 0.441130
SMoptNP =0.125 
c4NP = 37.253799
b4NP =   0.047130
nNP=   0.120450

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_10more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_10more$meanST5
meanGPP_P = years_sum2_10more$meanGPP
GPPmax_P = max(years_sum2_10more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_10more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.538066   0.038325  14.040   <2e-16 ***
#  c4    -13.303724   1.491857  -8.918   <2e-16 ***
#  b4      0.053002   0.002072  25.576   <2e-16 ***
#  n       0.193845   0.010950  17.703   <2e-16 ***
  
# Pulse parameters
FrefP =  0.538066
SMoptP =0.125 
c4P = -13.303724   
b4P =  0.053002
nP= 0.193845


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


############################################
# 11% Threshold #############################
############################################

years_sum2_11more <- years_sum2 %>%
  filter(Threshold_11 == 1)

years_sum2_11less <- years_sum2 %>%
  filter(Threshold_11 == 0)

# Assign variables
SoilMoisture <- years_sum2_11less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_11less$meanST5
meanGPP_NP <- years_sum2_11less$meanGPP
GPPmax_NP <- max(years_sum2_11less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_11less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.723618   0.035274  20.514  < 2e-16 ***
#  c4     37.670956   4.819081   7.817 9.94e-15 ***
#  b4      0.045547   0.001728  26.364  < 2e-16 ***
#  n       0.087192   0.003643  23.935  < 2e-16 ***

FrefNP = 0.723618
SMoptNP =0.125 
c4NP = 37.670956
b4NP =   0.045547
nNP=   0.087192

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_11more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_11more$meanST5
meanGPP_P = years_sum2_11more$meanGPP
GPPmax_P = max(years_sum2_11more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_11more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.453840   0.035866  12.654  < 2e-16 ***
#  c4    -12.854092   1.572078  -8.176 7.67e-16 ***
#  b4      0.055531   0.002213  25.089  < 2e-16 ***
#  n       0.247165   0.015673  15.770  < 2e-16 ***

# Pulse parameters
FrefP =  0.453840
SMoptP =0.125 
c4P = -12.854092   
b4P =  0.055531
nP= 0.247165


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


############################################
# 12% Threshold #############################
############################################

years_sum2_12more <- years_sum2 %>%
  filter(Threshold_12 == 1)

years_sum2_12less <- years_sum2 %>%
  filter(Threshold_12 == 0)

# Assign variables
SoilMoisture <- years_sum2_12less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_12less$meanST5
meanGPP_NP <- years_sum2_12less$meanGPP
GPPmax_NP <- max(years_sum2_12less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_12less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.723618   0.035274  20.514  < 2e-16 ***
#  c4     37.670956   4.819081   7.817 9.94e-15 ***
#  b4      0.045547   0.001728  26.364  < 2e-16 ***
#  n       0.087192   0.003643  23.935  < 2e-16 ***

FrefNP = 0.723618
SMoptNP =0.125 
c4NP = 37.670956
b4NP =   0.045547
nNP=   0.087192

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_12more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_12more$meanST5
meanGPP_P = years_sum2_12more$meanGPP
GPPmax_P = max(years_sum2_12more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_12more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.394224   0.035119  11.225  < 2e-16 ***
#  c4    -13.583853   1.700831  -7.987 3.91e-15 ***
#  b4      0.057113   0.002425  23.551  < 2e-16 ***
#  n       0.295316   0.020953  14.094  < 2e-16 ***

# Pulse parameters
FrefP =  0.394224
SMoptP =0.125 
c4P = -13.583853   
b4P =  0.057113 
nP= 0.295316 


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")



############################################
# 13% Threshold #############################
############################################

years_sum2_13more <- years_sum2 %>%
  filter(Threshold_13 == 1)

years_sum2_13less <- years_sum2 %>%
  filter(Threshold_13 == 0)

# Assign variables
SoilMoisture <- years_sum2_13less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_13less$meanST5
meanGPP_NP <- years_sum2_13less$meanGPP
GPPmax_NP <- max(years_sum2_13less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_13less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.827747   0.036679  22.567   <2e-16 ***
#  c4     42.445272   4.788010   8.865   <2e-16 ***
#  b4      0.043461   0.001561  27.835   <2e-16 ***
#  n       0.083195   0.003152  26.392   <2e-16 ***
  
FrefNP = 0.827747
SMoptNP =0.125 
c4NP = 42.445272
b4NP =  0.043461
nNP=   0.083195

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_13more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_13more$meanST5
meanGPP_P = years_sum2_13more$meanGPP
GPPmax_P = max(years_sum2_13more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_13more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.346948   0.033632  10.316  < 2e-16 ***
#  c4    -12.913264   1.800478  -7.172 1.62e-12 ***
#  b4      0.059790   0.002604  22.965  < 2e-16 ***
#  n       0.334453   0.025826  12.950  < 2e-16 ***

# Pulse parameters
FrefP =  0.346948
SMoptP =0.125 
c4P = -12.913264   
b4P =  0.059790
nP= 0.334453


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


############################################
# 14% Threshold #############################
############################################

years_sum2_14more <- years_sum2 %>%
  filter(Threshold_14 == 1)

years_sum2_14less <- years_sum2 %>%
  filter(Threshold_14 == 0)

# Assign variables
SoilMoisture <- years_sum2_14less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_14less$meanST5
meanGPP_NP <- years_sum2_14less$meanGPP
GPPmax_NP <- max(years_sum2_14less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_14less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.850762   0.035806  23.760   <2e-16 ***
#  c4     41.858798   4.786463   8.745   <2e-16 ***
#  b4      0.042274   0.001489  28.390   <2e-16 ***
#  n       0.084557   0.003067  27.567   <2e-16 ***

FrefNP = 0.850762
SMoptNP =0.125 
c4NP = 41.858798
b4NP =   0.042274
nNP=   0.084557

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_14more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_14more$meanST5
meanGPP_P = years_sum2_14more$meanGPP
GPPmax_P = max(years_sum2_14more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_14more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP   0.336558   0.035865   9.384  < 2e-16 ***
#  c4    -11.197551   1.943740  -5.761 1.25e-08 ***
#  b4      0.060197   0.002876  20.929  < 2e-16 ***
#  n       0.359006   0.030188  11.892  < 2e-16 ***

# Pulse parameters
FrefP =  0.336558
SMoptP =0.125 
c4P = -11.197551  
b4P =  0.060197
nP= 0.359006


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


############################################
# 15% Threshold #############################
############################################

years_sum2_15more <- years_sum2 %>%
  filter(Threshold_15 == 1)

years_sum2_15less <- years_sum2 %>%
  filter(Threshold_15 == 0)

# Assign variables
SoilMoisture <- years_sum2_15less$meanSWC5/100

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum2_15less$meanST5
meanGPP_NP <- years_sum2_15less$meanGPP
GPPmax_NP <- max(years_sum2_15less$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum2_15less,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#FrefNP  0.896844   0.036463  24.596  < 2e-16 ***
#  c4     35.724636   4.772225   7.486 1.04e-13 ***
#  b4      0.040524   0.001451  27.930  < 2e-16 ***
#  n       0.083102   0.002915  28.511  < 2e-16 ***

FrefNP = 0.896844
SMoptNP =0.125 
c4NP = 35.724636
b4NP =  0.040524
nNP=   0.083102

########## Fit Pulse model ##################

# Assign variables
SoilMoisture_P = years_sum2_15more$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum2_15more$meanST5
meanGPP_P = years_sum2_15more$meanGPP
GPPmax_P = max(years_sum2_15more$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum2_15more,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#FrefP  0.317582   0.037347   8.504  < 2e-16 ***
#  c4    -9.670967   2.076913  -4.656 3.98e-06 ***
#  b4     0.060625   0.003162  19.173  < 2e-16 ***
#  n      0.400895   0.037033  10.825  < 2e-16 ***

# Pulse parameters
FrefP =  0.317582
SMoptP =0.125 
c4P = -9.670967   
b4P =  0.060625
nP= 0.400895


#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)


# Plot the RECO time series 
plot(years_sum2$date, years_sum2$meanRECO, #type = "p", 
     col = "blue", xlab = "Year", ylab = "Reco", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum2$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum2$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")

###################### Next step - Combined model ################################
##################################################################################

# Create df just with fluxes and create combined Model
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)

Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4

Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM, MeanM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                   max_pulse_duration == 8 ~ PulseM,
                   max_pulse_duration == 14 ~ PulseM,
                   max_pulse_duration == 20 ~ PulseM))

Reco_Measured = sum(Reco1$meanRECO, na.rm = TRUE)
Reco_PandNP = sum(Reco1$`case_when(...)`, na.rm = TRUE)
Reco_MeanMod = sum(Reco1$MeanM, na.rm = TRUE)


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", 
     ylab =  "Reco, µmol m-2 s-1", cex = 0.8)

points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "Combined model", "Mean model"),
       pch = c(1, 16, 16),
       col = c("blue", "green", "red"),
       lty = c(NA, 1,1),
       bty = "n")


Reco1$Reco_Combined <- Reco1$`case_when(...)`


##### Find model parameters for each year ######################
################################################################

# NP-model
years_sum2_5less$year <- substr(years_sum2_5less$date, 1,4)
years_sum2_5less$year <- as.numeric(as.character(years_sum2_5less$year))
years_sum2_5less <- years_sum2_5less[-c(178),]

yearID1 <- unique(years_sum2_5less$year)

start1 <- list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre1 <- data.frame(matrix(nrow = length(yearID1), ncol = 1+length(start1)))
names(params.pre1) <- c("yearID1", names(start1))

for(i in seq_along(yearID1)) {
  # create data frame for sub "i"
  
  individual_DFs1 <- years_sum2_5less %>% filter (year %in% yearID1[i])
  
  # fit model for each sub "i"
  Param_model4_NP1 <- nlsLM(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                              (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
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
years_sum2_5more$year <- substr(years_sum2_5more$date, 1,4)
years_sum2_5more$year <- as.numeric(as.character(years_sum2_5more$year))

yearID <- unique(years_sum2_5more$year)

start <- list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre <- data.frame(matrix(nrow = length(yearID), ncol = 1+length(start)))
names(params.pre) <- c("yearID", names(start))


for(i in seq_along(yearID)) {
  # create data frame for sub "i"
  
  individual_DFs <- years_sum2_5more%>% filter (year %in% yearID[i])
  
  # fit model for each sub "i"
  Param_model4_P1 <- nlsLM(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n )/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
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
years_sum1$year <- substr(years_sum1$date, 1,4)
years_sum1$year <- as.numeric(as.character(years_sum1$year))

yearID2 <- unique(years_sum1$year)

start2 <- list(FrefL=0.75,  c4L=56.54, b4L=0.04, nL=0.84)

# create empty data.frame to store IDs and parameters
params.pre2 <- data.frame(matrix(nrow = length(yearID2), ncol = 1+length(start2)))
names(params.pre2) <- c("yearID", names(start2))


for(i in seq_along(yearID2)) {
  # create data frame for sub "i"
  
  individual_DFs2 <- years_sum1 %>% filter (year %in% yearID2[i])
  
  # fit model for each sub "i"
  Param_model4_All1 <- nlsLM(meanRECO ~ FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL)*(1-c4L*(0.1-All_meanSWC5)^2)*exp(b4L*All_meanST5), 
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


# NP-model - 10% SWC ##########
###############################

years_sum2_10less$year <- substr(years_sum2_10less$date, 1,4)
years_sum2_10less$year <- as.numeric(as.character(years_sum2_10less$year))

yearID1 <- unique(years_sum2_10less$year)

start1 <- list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre1 <- data.frame(matrix(nrow = length(yearID1), ncol = 1+length(start1)))
names(params.pre1) <- c("yearID1", names(start1))

for(i in seq_along(yearID1)) {
  # create data frame for sub "i"
  
  individual_DFs1 <- years_sum2_10less %>% filter (year %in% yearID1[i])
  
  # fit model for each sub "i"
  Param_model4_NP1 <- nlsLM(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                              (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
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
years_sum2_10more$year <- substr(years_sum2_10more$date, 1,4)
years_sum2_10more$year <- as.numeric(as.character(years_sum2_10more$year))

yearID <- unique(years_sum2_10more$year)

start <- list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre <- data.frame(matrix(nrow = length(yearID), ncol = 1+length(start)))
names(params.pre) <- c("yearID", names(start))


for(i in seq_along(yearID)) {
  # create data frame for sub "i"
  
  individual_DFs <- years_sum2_10more%>% filter (year %in% yearID[i])
  
  # fit model for each sub "i"
  Param_model4_P1 <- nlsLM(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n )/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
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


# NP-model - 15% SWC ##########
###############################

years_sum2_15less$year <- substr(years_sum2_15less$date, 1,4)
years_sum2_15less$year <- as.numeric(as.character(years_sum2_15less$year))

yearID1 <- unique(years_sum2_15less$year)

start1 <- list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre1 <- data.frame(matrix(nrow = length(yearID1), ncol = 1+length(start1)))
names(params.pre1) <- c("yearID1", names(start1))

for(i in seq_along(yearID1)) {
  # create data frame for sub "i"
  
  individual_DFs1 <- years_sum2_15less %>% filter (year %in% yearID1[i])
  
  # fit model for each sub "i"
  Param_model4_NP1 <- nlsLM(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                              (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
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

start <- list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre <- data.frame(matrix(nrow = length(yearID), ncol = 1+length(start)))
names(params.pre) <- c("yearID", names(start))


for(i in seq_along(yearID)) {
  # create data frame for sub "i"
  
  individual_DFs <- years_sum2_15more%>% filter (year %in% yearID[i])
  
  # fit model for each sub "i"
  Param_model4_P1 <- nlsLM(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n )/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
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


# calculate RMSE MEAN MODEL
rmse_MeanMod <- sqrt(sum((Reco1$MeanM - Reco1$meanRECO)^2, na.rm=TRUE)/nrow(Reco1))

# calculate MAPE -  Mean absolute percent error
mape_MeanMod <- mean(abs((Reco1$MeanM - Reco_df$meanRECO) / Reco1$meanRECO), na.rm=TRUE) * 100

# calculate R-squared
r_squared_MeanMod <- cor(Reco1$MeanM, Reco1$meanRECO, use = "complete.obs")^2

broom:: glance(Param_model4_All)

# calculate RMSE COMBINED MODEL
rmse_MeanMod <- sqrt(sum((Reco1$Reco_Combined - Reco1$meanRECO)^2, na.rm=TRUE)/nrow(Reco1))

# calculate MAPE -  Mean absolute percent error
mape_MeanMod <- mean(abs((Reco1$Reco_Combined - Reco_df$meanRECO) / Reco1$meanRECO), na.rm=TRUE) * 100

# calculate R-squared
r_squared_MeanMod <- cor(Reco1$Reco_Combined, Reco1$meanRECO, use = "complete.obs")^2


# calculate RMSE PULSE MODEL
rmse_MeanMod <- sqrt(sum((Reco1$PulseM - Reco1$meanRECO)^2, na.rm=TRUE)/nrow(Reco1))

# calculate MAPE -  Mean absolute percent error
mape_MeanMod <- mean(abs((Reco1$PulseM - Reco_df$meanRECO) / Reco1$meanRECO), na.rm=TRUE) * 100

# calculate R-squared
r_squared_MeanMod <- cor(Reco1$PulseM, Reco1$meanRECO, use = "complete.obs")^2

broom:: glance(Param_model4_P)

# calculate RMSE NON-PULSE MODEL
rmse_MeanMod <- sqrt(sum((Reco1$NonPulseM - Reco1$meanRECO)^2, na.rm=TRUE)/nrow(Reco1))

# calculate MAPE -  Mean absolute percent error
mape_MeanMod <- mean(abs((Reco1$NonPulseM - Reco_df$meanRECO) / Reco1$meanRECO), na.rm=TRUE) * 100

# calculate R-squared
r_squared_MeanMod <- cor(Reco1$NonPulseM, Reco1$meanRECO, use = "complete.obs")^2

broom:: glance(Param_model4_NP)


############## Cumulative fluxes ###########################
############################################################

Recodf_new <- Reco1 %>%
  na.omit() %>%
  select (date, meanRECO, MeanM, Reco_Combined)

Recodf_new$culMeasured <- ave(Recodf_new$meanRECO, FUN = cumsum)  
Recodf_new$culMeanMod <- ave(Recodf_new$MeanM, FUN = cumsum)  
Recodf_new$culModelled <- ave(Recodf_new$Reco_Combined, FUN = cumsum)  



plot(Recodf_new$date,Recodf_new$culMeasured,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Cumulative Reco", cex = 0.8)
lines(Recodf_new$date, Recodf_new$culMeanMod, type = "l", col = "red")
lines(Recodf_new$date, Recodf_new$culModelled, type = "l", col = "green")

legend(x = "topleft",
       legend = c("Measured Reco", "Mean model", "Combined model"),
       pch = c(1, 16, 16),
       col = c("blue", "red", "green"),
       lty = c(NA, 1,1),
       bty = "n")


Recodf_new$diffMean <- Recodf_new$culMeasured - Recodf_new$culMeanMod
Recodf_new$diffComb <- Recodf_new$culMeasured - Recodf_new$culModelled


plot(Recodf_new$date,Recodf_new$diffComb,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-60,200)
)
lines(Recodf_new$date, Recodf_new$diffMean, type = "l", col = "red")

legend(x = "topleft",
       legend = c("Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")


Recodf_new %>%
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


Recodf_new$diffMeanV <- Recodf_new$meanRECO - Recodf_new$MeanM
Recodf_new$diffCombV <- Recodf_new$meanRECO - Recodf_new$Reco_Combined


plot(Recodf_new$date,Recodf_new$diffCombV,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-2,3)
)
lines(Recodf_new$date, Recodf_new$diffMeanV, type = "l", col = "red")

legend(x = "topleft",
       legend = c( "Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")


### Calculate differences from the measured values Modelled - Measured  #########

Recodf_new$diffMean2 <- Recodf_new$culMeanMod - Recodf_new$culMeasured  
Recodf_new$diffComb2 <- Recodf_new$culModelled - Recodf_new$culMeasured 


plot(Recodf_new$date,Recodf_new$diffComb2,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-200,50)
)
lines(Recodf_new$date, Recodf_new$diffMean2, type = "l", col = "red")

legend(x = "topleft",
       legend = c("Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")


Recodf_new %>%
  ggplot(aes(x=date))+ 
  geom_line(aes(y = diffMean2, col = 'diffMean'))+
  geom_line(aes(y = diffComb2, col = 'diffComb'))+
  theme_classic()+
  theme(text = element_text(size = 15))+
  #stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  #stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Modelled Rsoil, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Year')
#ggtitle('Non-pulse time')+
#ylim(0,4)+
#xlim(0,4)


Recodf_new$diffMeanV2 <- Recodf_new$MeanM - Recodf_new$meanRECO
Recodf_new$diffCombV2 <- Recodf_new$Reco_Combined - Recodf_new$meanRECO 


plot(Recodf_new$date,Recodf_new$diffCombV2,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-2,3)
)
lines(Recodf_new$date, Recodf_new$diffMeanV2, type = "l", col = "red")

legend(x = "topleft",
       legend = c( "Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")























