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


plot(Rsoil1$date, Rsoil1$meanRsoil, type = "p", col = "blue", xlab = "Year", 
     ylab =  "Rsoil, µmol m-2 s-1", cex = 0.8,
     ylim = c(0,4))

points(Rsoil1$date, Rsoil1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Rsoil1$date, Rsoil1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)

# create the legend
legend(x = "topleft",
       legend = c("Measured Rsoil", "Combined model", "Mean model"),
       pch = c(1, 16),
       col = c("blue", "green", "red"),
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


plot(Rsoildf_new$date,Rsoildf_new$culMeasured,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Cumulative Rsoil", cex = 0.8)
lines(Rsoildf_new$date, Rsoildf_new$culMeanMod, type = "l", col = "red")
lines(Rsoildf_new$date, Rsoildf_new$culCombined, type = "l", col = "green")


legend(x = "topleft",
       legend = c("Measured Rsoil", "Mean model", "Combined model" ),
       pch = c(1, 16, 16),
       col = c("blue", "red", "green"),
       lty = c(NA, 1,1),
       bty = "n")



sum(Rsoildf_new$meanRsoil)
sum(Rsoildf_new$MeanM)
sum(Rsoildf_new$Rsoil_Combined)

######### Difference Rsoil Measured - Modelled 
Rsoildf_new$diffMean <- Rsoildf_new$culMeasured - Rsoildf_new$culMeanMod
Rsoildf_new$diffComb <- Rsoildf_new$culMeasured - Rsoildf_new$culCombined


plot(Rsoildf_new$date,Rsoildf_new$diffComb,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-60,80)
     )
lines(Rsoildf_new$date, Rsoildf_new$diffMean, type = "l", col = "red")

legend(x = "topleft",
       legend = c("Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
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


plot(Rsoildf_new$date,Rsoildf_new$diffCombV,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-3,6)
)
lines(Rsoildf_new$date, Rsoildf_new$diffMeanV, type = "l", col = "red")

legend(x = "topleft",
       legend = c( "Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")

######### Difference Modelled - Rsoil Measured 
Rsoildf_new$diffMean2 <-  Rsoildf_new$culMeanMod - Rsoildf_new$culMeasured
Rsoildf_new$diffComb2 <- Rsoildf_new$culCombined - Rsoildf_new$culMeasured 


plot(Rsoildf_new$date,Rsoildf_new$diffComb2,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-80,60)
)
lines(Rsoildf_new$date, Rsoildf_new$diffMean2, type = "l", col = "red")

legend(x = "topleft",
       legend = c("Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
       bty = "n")


Rsoildf_new %>%
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


Rsoildf_new$diffMeanV2 <- Rsoildf_new$MeanM - Rsoildf_new$meanRsoil 
Rsoildf_new$diffCombV2 <- Rsoildf_new$Rsoil_Combined - Rsoildf_new$meanRsoil 


plot(Rsoildf_new$date,Rsoildf_new$diffCombV2,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Rsoil", #cex = 0.8,
     ylim = c(-6,3)
)
lines(Rsoildf_new$date, Rsoildf_new$diffMeanV2, type = "l", col = "red")

legend(x = "topleft",
       legend = c( "Combined model difference", "Mean model difference"),
       pch = c(1, 16, 16),
       col = c("blue", "red"),
       lty = c(NA, 1, 1),
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





