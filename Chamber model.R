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

# Open new files 
summary_Cham_P <- read.csv("data/All summary chamber.csv")
Pulse_Cham <- read.csv("data/Pulse sum chamber.csv")
NonPulse_Cham <- read.csv("data/NonPulse sum chamber.csv")

# Make sure in the Date format
NonPulse_Cham$date <- as.Date(NonPulse_Cham$date)
Pulse_Cham$date <- as.Date(Pulse_Cham$date)
summary_Cham_P$date <- as.Date(summary_Cham_P$date)

# CHECK COLUMN NAMES AND DATA TYPES
str(NonPulse_Cham)
str(Pulse_Cham)
str(summary_Cham_P)

###### Pulse time model ###########################
###################################################

# Assign variables
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
FrefP = 0.446910
SMoptP =0.125 
c4P = -1.058658   
b4P = 0.044799
nP= 0.298964

####### Non-Pulse model ###################
###########################################

# Assign variables
meanSWC_NP <- NonPulse_Cham$meanSWC
meanST_NP <- NonPulse_Cham$meanTsoil
meanGPP_NP <- NonPulse_Cham$meanGPP
GPPmax_NP <- max(NonPulse_Cham$meanGPP, na.rm = TRUE)

# Fit model
Param_model4_NP <- nls(meanRsoil ~ Fref*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                         (1-c4*(0.1-meanSWC_NP)^2)*exp(b4*meanST_NP), 
                       data = NonPulse_Cham,
                       start = list(Fref=0.75, c4=56.54, b4=0.04, n=0.84),
                       control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_NP = summary(Param_model4_NP)

# Formula: meanRECO ~ Fref * ((meanGPP_NP/GPPmax_NP + n)/1 + n) * (1 - c4 * 

# NON_Pulse parameters
FrefNP = 0.743454
SMoptNP =0.125 
c4NP = -1.668911  
b4NP = 0.023544
nNP=  0.160941

############### All time model #################################
################################################################

complete.cases(summary_Cham_P)

# Setting up drivers for all time
All_meanSWC = summary_Cham_P$meanSWC
All_meanST = summary_Cham_P$meanTsoil
All_meanGPP = summary_Cham_P$meanGPP
All_GPPmax = max(summary_Cham_P$meanGPP, na.rm = TRUE)

Param_model4_All <- nls(meanRsoil ~ Fref*((All_meanGPP/All_GPPmax +n)/1+n) *(1-c4*(0.1-All_meanSWC)^2)*exp(b4*All_meanST), 
                        data = summary_Cham_P,
                        start = list(Fref=0.75,  c4=56.54, b4=0.04, n=0.84),
                        control = nls.control(maxiter = 1000, minFactor = 0.01)
)
Summary_Model4_All = summary(Param_model4_All)

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
plot(summary_Cham_P$date, summary_Cham_P$meanRsoil, type = "p", col = "blue", xlab = "Timestamp", ylab = "Rsoil", cex = 0.8)

# Add the model output time series to the plot - CORRECT FIGURE
points(summary_Cham_P$date, ALL_model4_NP, col = "red", pch = 16, cex = 0.4)
points(summary_Cham_P$date, All_model4_P, col = "cyan", pch = 16, cex = 0.4, alpha=0.5)
points(summary_Cham_P$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured Rsoil", "Pulse Model", "Non-Pulse Model", "Mean model"),
       pch = c(1, 16, 16,16),
       col = c("blue", "cyan", "red","green"),
       lty = c(NA, 1, 1,1),
       bty = "n")


