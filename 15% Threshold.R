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


Recodf_new15$diffMean <- Recodf_new15$culMeanMod - Recodf_new15$culMeasured 
Recodf_new15$diffComb <- Recodf_new15$culModelled - Recodf_new15$culMeasured 


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


Recodf_new15$diffMeanV <- Recodf_new15$MeanM_15 - Recodf_new15$meanRECO 
Recodf_new15$diffCombV <- Recodf_new15$Reco_Combined - Recodf_new15$meanRECO


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

