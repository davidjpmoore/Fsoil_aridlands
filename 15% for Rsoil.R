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
  select(c(2:7 | 9:10))


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
points(years_sum2$date, All_model4, col = "green", pch = 16, cex = 0.4, alpha=0.5)

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
Rsoil_df15$MeanM_15 <- All_model4

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


