# 17-05-2024
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


# Read other pulse division docs - with "DM" in their names
years_sum1 <- read.csv("data/years_sum1_DM.csv")
years_sum_Pulse0 <- read.csv("data/years_sum_Pulse0_DM.csv")
years_sum_Pulse1 <- read.csv("data/years_sum_Pulse1_DM.csv")

# Make sure in the Date format
years_sum_Pulse0$date <- as.Date(years_sum_Pulse0$date)
years_sum_Pulse1$date <- as.Date(years_sum_Pulse1$date)
years_sum1$date <- as.Date(years_sum1$date)

# CHECK COLUMN NAMES AND DATA TYPES
str(years_sum_Pulse0)
str(years_sum_Pulse1)
str(years_sum1)

####### Non-Pulse model ###################
###########################################

# Remove rows with missing values
years_sum_Pulse0 <- na.omit(years_sum_Pulse0)

# Calculate Soil Moisture
years_sum_Pulse0$meanSWC5 <- as.numeric(as.character(years_sum_Pulse0$meanSWC5))
SoilMoisture <- years_sum_Pulse0$meanSWC5/100

# Assign variables
years_sum_Pulse0$meanST5 <- as.numeric(as.character(years_sum_Pulse0$meanST5))
years_sum_Pulse0$meanGPP <- as.numeric(as.character(years_sum_Pulse0$meanGPP))

meanSWC5_NP <- SoilMoisture
meanST5_NP <- years_sum_Pulse0$meanST5
meanGPP_NP <- years_sum_Pulse0$meanGPP
GPPmax_NP <- max(years_sum_Pulse0$meanGPP, na.rm = TRUE)

########## Fit Non-Pulse model ##################
years_sum_Pulse0$meanRECO <- as.numeric(as.character(years_sum_Pulse0$meanRECO))

Param_model4_NP <- nls(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum_Pulse0,
                       start = list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# Formula: meanRECO ~ Fref * ((meanGPP_NP/GPPmax_NP + n)/1 + n) * (1 - c4 * 
#                                                                    (0.1 - meanSWC5_NP)^2) * exp(b4 * meanST5_NP)

# NON_Pulse parameters
#Estimate Std. Error t value Pr(>|t|)    
#Fref 0.561731   0.022309  25.179   <2e-16 ***
# c4   8.106202   4.970882   1.631    0.103    
#b4   0.034162   0.001555  21.973   <2e-16 ***
# n    0.129719   0.004816  26.933   <2e-16 ***


FrefNP = 0.561731
FrefNP_SE = 0.022309
SMoptNP =0.125 
c4NP = 8.106202
c4NP_SE = 4.970882
b4NP =  0.034162 
b4NP_SE = 0.001555
nNP=  0.129719
nNP_SE = 0.004816


#################### Pulse model ##################################
###################################################################

# Remove rows with missing values
years_sum_Pulse1 <- na.omit(years_sum_Pulse1)

# Change the data format 
years_sum_Pulse1$meanSWC5 <- as.numeric(as.character(years_sum_Pulse1$meanSWC5))
years_sum_Pulse1$meanST5 <- as.numeric(as.character(years_sum_Pulse1$meanST5))
years_sum_Pulse1$meanGPP <- as.numeric(as.character(years_sum_Pulse1$meanGPP))
years_sum_Pulse1$meanRECO <- as.numeric(as.character(years_sum_Pulse1$meanRECO))

# Assign variables
SoilMoisture_P =years_sum_Pulse1$meanSWC5/100
meanSWC5_P = SoilMoisture_P
meanST5_P = years_sum_Pulse1$meanST5
meanGPP_P = years_sum_Pulse1$meanGPP
GPPmax_P = max(years_sum_Pulse1$meanGPP, na.rm=TRUE)

Param_model4_P <- nls(meanRECO ~ FrefP*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum_Pulse1,
                      start = list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_P = summary(Param_model4_P)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# Fref   0.616634   0.045269  13.621  < 2e-16 ***
#   c4   -10.431847   1.596214  -6.535 1.01e-10 ***
#   b4     0.044407   0.002129  20.862  < 2e-16 ***
#   n      0.243863   0.014703  16.586  < 2e-16 ***

# Pulse parameters
FrefP = 0.616634 
FrefP_SE = 0.045269
SMoptP =0.125 
c4P = -10.431847   
c4P_SE = 1.596214
b4P = 0.044407
b4P_SE = 0.002129
nP= 0.243863
nP_SE = 0.014703


############### All time model OR MEAN MODEL #################################
################################################################

complete.cases(years_sum1)

# Remove rows with missing values
years_sum1 <- na.omit(years_sum1)

# Change data format
years_sum1$meanSWC5 <- as.numeric(as.character(years_sum1$meanSWC5))
years_sum1$meanST5 <- as.numeric(as.character(years_sum1$meanST5))
years_sum1$meanGPP <- as.numeric(as.character(years_sum1$meanGPP))
years_sum1$meanRECO <- as.numeric(as.character(years_sum1$meanRECO))

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
FrefL_SE = 0.049023
SMoptL =0.125 
c4L = -7.755161   
c4L_SE = 1.219586
b4L = 0.036016
b4L_SE = 0.001490
nL= 0.079803
nL_SE = 0.002769

#run model for full time series
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)

# Plot the RECO time series 
plot(years_sum1$date, years_sum1$meanRECO, type = "p", col = "blue", xlab = "Timestamp", ylab = "RECO", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(years_sum1$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(years_sum1$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(years_sum1$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


##### Compare modeled and measured data
plot(years_sum1$meanRECO,ALL_model4_NP, type = "p", col = "red", xlab = "MEASURED RECO", ylab = "Non-pulse model RECO")
text(x = 1, y = 1.5, labels = "Over estimation")
text(x = 3, y = 0.5, labels = "Under estimation")
points( years_sum1$meanRECO,years_sum1$meanRECO,  type = "l", col = "red")

plot(years_sum1$meanRECO,All_model4_P,  type = "p", col = "cyan", xlab = "MEASURED RECO", ylab = "Pulse model RECO")
text(x = 1, y = 2.5, labels = "Over estimation")
text(x = 3, y = 0.5, labels = "Under estimation")
points( years_sum1$meanRECO,years_sum1$meanRECO,  type = "l", col = "cyan")



# Calulate the model residual and investigate whether residuals are higher during pulse times. 
Model_residual_NP = years_sum1$meanRECO-ALL_model4_NP
hist(Model_residual_NP, col = "red")

Model_residual_P = years_sum1$meanRECO - All_model4_P
hist(Model_residual_P, col = "cyan")


# calculate RMSE
rmse_NP <- sqrt(mean((ALL_model4_NP - years_sum1$meanRECO)^2, na.rm = TRUE))
rmse_P <- sqrt(mean((All_model4_P - years_sum1$meanRECO)^2, na.rm = TRUE))
# calculate MAPE
mape_NP <- mean(abs(ALL_model4_NP - years_sum1$meanRECO) / years_sum1$meanRECO, na.rm = TRUE) * 100
mape_P <- mean(abs(All_model4_P - years_sum1$meanRECO) / years_sum1$meanRECO, na.rm = TRUE) * 100
# calculate R-squared
r_squared_NP <- cor(ALL_model4_NP, years_sum1$meanRECO, use = "complete.obs")^2
r_squared_P <- cor(All_model4_P, years_sum1$meanRECO, use = "complete.obs")^2

Reco_NP = sum(ALL_model4_NP, na.rm = TRUE)
Reco_P = sum(All_model4_P, na.rm = TRUE)
Reco_obs = sum(years_sum1$meanRECO, na.rm = TRUE)
Reco_allMean = sum(All_model4, na.rm=TRUE)


plot(ALL_model4_NP,All_model4_P, xlab = "Non-Pulse Model Reco", ylab = "Pulse Model Reco")
 
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

Reco_df %>%
  na.omit() %>%
  ggplot(aes(x=date))+
  geom_point(aes(y = meanRECO),shape=20, color = "blue", size = 2)+
  geom_point(aes(y=Reco_Combined),shape=1, size = 1, color = "green")+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Timestamp")+
  ylim(0,4)
  
plot(Reco_df$meanRECO, Reco_df$Reco_Combined, type = "p", col = "black", xlab = "Mesured Reco", 
     ylab =  "Modelled Reco", cex = 0.8)

Reco_df %>%
  ggplot(aes(x=meanRECO, y = Reco_Combined))+
  geom_point(shape=1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Combined model Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("Measured Reco, ", mu, "mol m"^-2,"s"^-1))+
  ylim(0,5)+
  xlim(0,5)

# calculate RMSE
rmse_CombinedMod <- sqrt(sum((Reco_df$Reco_Combined - Reco_df$meanRECO)^2, na.rm=TRUE)/nrow(Reco_df))

# calculate MAPE -  Mean absolute percent error
mape_CombinedMod <- mean(abs((Reco_df$Reco_Combined - Reco_df$meanRECO) / Reco_df$meanRECO), na.rm=TRUE) * 100
 
# calculate R-squared
r_squared_CombinedMod <- cor(Reco_df$Reco_Combined, Reco_df$meanRECO, use = "complete.obs")^2


###### Calculate cumulative flux for - Mean model, Including Pulse and non-pulse together and measured fluxes ######
####################################################################################################################

### Create df with just fluxes #####

Recodf_new <- Reco_df %>%
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


















