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

# Open file with all eddy-covariance data 
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

# Fit model
years_sum_Pulse0$meanRECO <- as.numeric(as.character(years_sum_Pulse0$meanRECO))

Param_model4_NP <- nls(meanRECO ~ Fref*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                       data = years_sum_Pulse0,
                       start = list(Fref=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# Formula: meanRECO ~ Fref * ((meanGPP_NP/GPPmax_NP + n)/1 + n) * (1 - c4 * 
#                                                                    (0.1 - meanSWC5_NP)^2) * exp(b4 * meanST5_NP)
# 
#Estimated Wed May 3rd 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# Fref     0.021298  25.796  < 2e-16 ***
#   c4   14.410701   4.860611   2.965  0.00307 ** 
#   b4    0.034738   0.001528  22.737  < 2e-16 ***
#   n     0.131463   0.004753  27.661  < 2e-16 ***
#   

# NON_Pulse parameters
FrefNP = 0.581979
FrefNP_SE = 0.022416
SMoptNP =0.125 
c4NP = 5.866561
c4NP_SE = 4.914379
b4NP =  0.032845
b4NP_SE = 0.001487
nNP=  0.129888
nNP_SE = 0.004721

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

Param_model4_P <- nls(meanRECO ~ Fref*((meanGPP_P/GPPmax_P +n)/1+n) *(1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                      data = years_sum_Pulse1,
                      start = list(Fref=0.75,  c4=56.54, b4=0.04, n=0.84),
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
FrefP = 0.646765 
FrefP_SE = 0.044472
SMoptP =0.125 
c4P = -10.166789   
c4P_SE = 1.489036
b4P = 0.043525
b4P_SE = 0.001986
nP= 0.236665
nP_SE = 0.013583

############### All time model #################################
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
All_meanSWC5 =years_sum1$meanSWC5/100
All_meanST5 = years_sum1$meanST5
All_meanGPP = years_sum1$meanGPP
All_GPPmax = max(years_sum1$meanGPP, na.rm = TRUE)

Param_model4_All <- nls(meanRECO ~ Fref*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4L*(0.1-All_meanSWC5)^2)*exp(b4L*All_meanST5), 
                      data = years_sum1,
                      start = list(Fref=0.75,  c4L=56.54, b4L=0.04, nL=0.84),
                      control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_All = summary(Param_model4_All)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# Fref   0.616634   0.045269  13.621  < 2e-16 ***
#   c4   -10.431847   1.596214  -6.535 1.01e-10 ***
#   b4     0.044407   0.002129  20.862  < 2e-16 ***
#   n      0.243863   0.014703  16.586  < 2e-16 ***

# Pulse parameters
FrefL = 1.183562
FrefL_SE = 0.048450
SMoptL =0.125 
c4L = -7.875260   
c4L_SE = 1.160614
b4L = 0.034806
b4L_SE = 0.001412
nL= 0.079416
nL_SE = 0.002654

#run model for full time series based on non-pulse time parameters
ALL_model4_NP = FrefNP*((All_meanGPP/All_GPPmax +nNP)/1+nNP) *(1-c4NP*(SMoptNP-All_meanSWC5)^2)*exp(b4NP*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)
#run all time model
All_model4 = FrefL*((All_meanGPP/All_GPPmax +nL)/1+nL) *(1-c4P*(SMoptL-All_meanSWC5)^2)*exp(b4L*All_meanST5)

# Plot the RECO time series
plot(years_sum1$date, years_sum1$meanRECO, type = "p", col = "blue", xlab = "Timestamp", ylab = "RECO", cex = 0.8)

points(years_sum1$date, results_df$MeanR, col="green", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Mean model"),
       pch = c(1, 16),
       col = c("blue", "green"),
       lty = c(NA, 1),
       bty = "n")


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
rmse_NP <- sqrt(mean((ALL_model4_NP - years_sum1$meanRECO)^2))
rmse_P <- sqrt(mean((All_model4_P - years_sum1$meanRECO)^2))
# calculate MAPE
mape_NP <- mean(abs(ALL_model4_NP - years_sum1$meanRECO) / years_sum1$meanRECO) * 100
mape_P <- mean(abs(All_model4_P - years_sum1$meanRECO) / years_sum1$meanRECO) * 100
# calculate R-squared
r_squared_NP <- cor(ALL_model4_NP, years_sum1$meanRECO)^2
r_squared_P <- cor(All_model4_P, years_sum1$meanRECO)^2

Reco_NP = sum(ALL_model4_NP)
Reco_P = sum(All_model4_P)
Reco_obs = sum(years_sum1$meanRECO)


plot(ALL_model4_NP,All_model4_P, xlab = "Non-Pulse Model Reco", ylab = "Pulse Model Reco")
 

# Create df just with fluxes and calculate 
Reco_df <- years_sum1 %>%
  select(date, meanRECO, sdReco, max_pulse_duration, rain_event)


Reco_df$PulseM <- All_model4_P
Reco_df$NonPulseM <- ALL_model4_NP
Reco_df$MeanM <- All_model4


Reco1 <- Reco_df %>%
  select (date, meanRECO, max_pulse_duration, PulseM, NonPulseM) %>%
  mutate(case_when(max_pulse_duration == 0 ~ NonPulseM,
                  max_pulse_duration == 8 ~ PulseM,
                max_pulse_duration == 14 ~ PulseM,
                max_pulse_duration == 20 ~ PulseM))
                         


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", ylab = "RECO", cex = 0.8)
points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured RECO", "Pulse and Non-pulse models"),
       pch = c(1, 16),
       col = c("blue", "green"),
       lty = c(NA, 1),
       bty = "n")






