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
library(minpack.lm)
library(modules)


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
SMoptNP = 0.125 
c4NP = 8.106202
b4NP =  0.034162 
nNP=  0.129719

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
SMoptP =0.125 
c4P = -10.431847   
b4P = 0.044407
nP= 0.243863


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


##### Find model parameters for each year ######################
################################################################

# NP-model
years_sum_Pulse0$year <- substr(years_sum_Pulse0$date, 1,4)
years_sum_Pulse0$year <- as.numeric(as.character(years_sum_Pulse0$year))

yearID1 <- unique(years_sum_Pulse0$year)

start1 <- list(FrefNP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre1 <- data.frame(matrix(nrow = length(yearID1), ncol = 1+length(start1)))
names(params.pre1) <- c("yearID1", names(start1))

for(i in seq_along(yearID1)) {
  # create data frame for sub "i"
  
  individual_DFs1 <- years_sum_Pulse0 %>% filter (year %in% yearID1[i])
  
  # fit model for each sub "i"
  Param_model4_NP1 <- nlsLM(meanRECO ~ FrefNP*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                              (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                           data = individual_DFs1,
                           start = start1, trace = TRUE,
                            )
  
  # store IDs
  params.pre1[i,1] <- yearID1[i]
  
  # store fit parameters
  params.pre1[i,2:ncol(params.pre1)] <- Param_model4_NP1$m$getPars()
  
  #params.pre[i,3:ncol(params.pre)] <- Param_model4_P1$m$getPars()
  
  
  
}

params.pre1


# Pulse model
years_sum_Pulse1$year <- substr(years_sum_Pulse1$date, 1,4)
years_sum_Pulse1$year <- as.numeric(as.character(years_sum_Pulse1$year))

yearID <- unique(years_sum_Pulse1$year)

start <- list(FrefP=0.75, c4=56.54, b4=0.04, n=0.84)

# create empty data.frame to store IDs and parameters
params.pre <- data.frame(matrix(nrow = length(yearID), ncol = 1+length(start)))
names(params.pre) <- c("yearID", names(start))


for(i in seq_along(yearID)) {
  # create data frame for sub "i"
  
  individual_DFs <- years_sum_Pulse1 %>% filter (year %in% yearID[i])
  
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


Reco15 <- read.csv("data/Reco15.csv")


Reco1$thresholdM <- Reco15$Reco_Combined


plot(Reco_df$date, Reco_df$meanRECO, type = "p", col = "blue", xlab = "Timestamp", ylab =  "Reco, µmol m-2 s-1", cex = 0.8)
     
points(Reco1$date, Reco1$`case_when(...)`, col="green", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$MeanM, col="red", pch = 16, cex = 0.4, alpha=0.5)
points(Reco1$date, Reco1$thresholdM, col="cyan", pch = 16, cex = 0.4, alpha=0.5)
# create the legend
legend(x = "topleft",
       legend = c("Measured Reco", "P-NP model", "Mean model", "15% model"),
       pch = c(1, 16, 16, 16),
       col = c("blue", "green", "red", "cyan"),
       lty = c(NA, 1,1,1),
       bty = "n")


Reco_df$Reco_Combined <- Reco1$`case_when(...)`


######################  Additional analysis ##############################

Reco1$STmean <- years_sum1$meanST5
Reco1$SWCmean <- years_sum1$meanSWC5
Reco1$Reco_Combined <- Reco1$`case_when(...)`


Reco2_NP <- Reco1 %>%
  filter(max_pulse_duration == 0) %>%
  select(meanRECO,MeanM, thresholdM, Reco_Combined, STmean)


Reco2_long_NP_Tsoil <- Reco2_NP %>%
  pivot_longer(!STmean, names_to = "Models", values_to = "values")


Reco2_long_NP_Tsoil %>%
  ggplot(aes(x=STmean, y=values, col = Models))+
  geom_point(shape = 1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Soil temperature, °C')+
  ylim(0,4) +
  xlim(0,40)+
  stat_regline_equation(aes(label = paste(..adj.rr.label.., sep = "~")),
                        size = 3
  )+
  stat_smooth(method = "nls"
              , formula = y ~  exp(  as.numeric(x)
              )
        #      , method.args = list( start = c( A = 0.8, B = 0.05 ) )
        #      , se=FALSE
  )+
  stat_cor(aes(label = paste(..p.label.., sep = "~")), 
          size = 3,
          label.x.npc = "centre", digits = 3, p.accuracy = 0.001)+
  ggtitle ('NP time')




Reco2_P <- Reco1 %>%
  filter(max_pulse_duration > 0) %>%
  select(meanRECO,MeanM, thresholdM, Reco_Combined, STmean)


Reco2_long_P_Tsoil <- Reco2_P %>%
  pivot_longer(!STmean, names_to = "Models", values_to = "values")


Reco2_long_P_Tsoil %>%
  ggplot(aes(x=STmean, y=values, col = Models))+
  geom_point(shape = 1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Soil temperature, °C')+
  ylim(0,4) +
  xlim(0,40)+
  stat_regline_equation(aes(label = paste(..adj.rr.label.., sep = "~")),
                        size = 3
  )+
  stat_smooth(method = "nls"
              , formula = y ~  exp(  as.numeric(x)
              )
              #      , method.args = list( start = c( A = 0.8, B = 0.05 ) )
              #      , se=FALSE
  )+
  stat_cor(aes(label = paste(..p.label.., sep = "~")), 
           size = 3,
           label.x.npc = "centre", digits = 3, p.accuracy = 0.001)+
  ggtitle ('P time')


Reco2_all <- Reco1 %>%
  #filter(max_pulse_duration > 0) %>%
  select(meanRECO,MeanM, thresholdM, Reco_Combined, STmean)


Reco2_long_all_Tsoil <- Reco2_all %>%
  pivot_longer(!STmean, names_to = "Models", values_to = "values")


Reco2_long_all_Tsoil %>%
  ggplot(aes(x=STmean, y=values, col = Models))+
  geom_point(shape = 1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Soil temperature, °C')+
  ylim(0,4) +
  xlim(0,40)+
  stat_regline_equation(aes(label = paste(..adj.rr.label.., sep = "~")),
                        size = 3
  )+
  stat_smooth(method = "nls"
              , formula = y ~  exp(  as.numeric(x)
              )
              #      , method.args = list( start = c( A = 0.8, B = 0.05 ) )
              #      , se=FALSE
  )+
  stat_cor(aes(label = paste(..p.label.., sep = "~")), 
           size = 3,
           label.x.npc = "centre", digits = 3, p.accuracy = 0.001)+
  ggtitle ('All time')



Reco3_NP <- Reco1 %>%
  filter(max_pulse_duration == 0) %>%
  select(meanRECO,MeanM, thresholdM, Reco_Combined, SWCmean)


Reco3_long_NP_SWC <- Reco3_NP %>%
  pivot_longer(!SWCmean, names_to = "Models", values_to = "values")


Reco3_long_NP_SWC %>%
  ggplot(aes(x=SWCmean, y=values, col = Models))+
  geom_point(shape = 1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Soil moisture, %')+
  ylim(0,4) +
  xlim(0,40)+
  stat_regline_equation(aes(label = paste(..adj.rr.label.., sep = "~")),
                        size = 3
  )+
  stat_smooth(method = "nls"
              , formula = y ~  poly(  as.numeric(x)
              )
              #      , method.args = list( start = c( A = 0.8, B = 0.05 ) )
              #      , se=FALSE
  )+
  stat_cor(aes(label = paste(..p.label.., sep = "~")), 
           size = 3,
           label.x.npc = "centre", digits = 3, p.accuracy = 0.001)+
  ggtitle ('NP time')




Reco3_P <- Reco1 %>%
  filter(max_pulse_duration > 0) %>%
  select(meanRECO,MeanM, thresholdM, Reco_Combined, SWCmean)


Reco3_long_P_SWC <- Reco3_P %>%
  pivot_longer(!SWCmean, names_to = "Models", values_to = "values")


Reco3_long_P_SWC %>%
  ggplot(aes(x=SWCmean, y=values, col = Models))+
  geom_point(shape = 1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Soil moisture, %')+
  ylim(0,4) +
  xlim(0,40)+
  stat_regline_equation(aes(label = paste(..adj.rr.label.., sep = "~")),
                        size = 3
  )+
  stat_smooth(method = "nls"
              , formula = y ~  poly(  as.numeric(x)
              )
              #      , method.args = list( start = c( A = 0.8, B = 0.05 ) )
              #      , se=FALSE
  )+
  stat_cor(aes(label = paste(..p.label.., sep = "~")), 
           size = 3,
           label.x.npc = "centre", digits = 3, p.accuracy = 0.001)+
  ggtitle ('P time')


Reco3_all <- Reco1 %>%
  #filter(max_pulse_duration > 0) %>%
  select(meanRECO,MeanM, thresholdM, Reco_Combined, SWCmean)


Reco3_long_all_SWC <- Reco3_all %>%
  pivot_longer(!SWCmean, names_to = "Models", values_to = "values")


Reco3_long_all_SWC %>%
  ggplot(aes(x=SWCmean, y=values, col = Models))+
  geom_point(shape = 1)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab('Soil moisture, %')+
  ylim(0,4) +
  xlim(0,40)+
  stat_regline_equation(aes(label = paste(..adj.rr.label.., sep = "~")),
                        size = 3
  )+
  stat_smooth(method = "nls"
              , formula = y ~  poly(  as.numeric(x)
              )
              #      , method.args = list( start = c( A = 0.8, B = 0.05 ) )
              #      , se=FALSE
  )+
  stat_cor(aes(label = paste(..p.label.., sep = "~")), 
           size = 3,
           label.x.npc = "centre", digits = 3, p.accuracy = 0.001)+
  ggtitle ('All time')



######################################################################################


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


# calculate RMSE COMBINED MODEL
rmse_CombinedMod <- sqrt(sum((Reco_df$Reco_Combined - Reco_df$meanRECO)^2, na.rm=TRUE)/nrow(Reco_df))

# calculate MAPE -  Mean absolute percent error
mape_CombinedMod <- mean(abs((Reco_df$Reco_Combined - Reco_df$meanRECO) / Reco_df$meanRECO), na.rm=TRUE) * 100
 
# calculate R-squared 
r_squared_CombinedMod <- cor(Reco_df$Reco_Combined, Reco_df$meanRECO, use = "complete.obs")^2

# calculate RMSE MEAN MODEL
rmse_MeanMod <- sqrt(sum((Reco_df$MeanM - Reco_df$meanRECO)^2, na.rm=TRUE)/nrow(Reco_df))

# calculate MAPE -  Mean absolute percent error
mape_MeanMod <- mean(abs((Reco_df$MeanM - Reco_df$meanRECO) / Reco_df$meanRECO), na.rm=TRUE) * 100

# calculate R-squared
r_squared_MeanMod <- cor(Reco_df$MeanM, Reco_df$meanRECO, use = "complete.obs")^2



###### Calculate cumulative flux for - Mean model, Including Pulse and non-pulse together and measured fluxes ######
####################################################################################################################

### Create df with just fluxes #####

Recodf_new <- Reco_df %>%
  na.omit() %>%
  select (date, meanRECO, MeanM, Reco_Combined)

Recodf_new$culMeasured <- ave(Recodf_new$meanRECO, FUN = cumsum)  
Recodf_new$culMeanMod <- ave(Recodf_new$MeanM, FUN = cumsum)  
Recodf_new$culModelled <- ave(Recodf_new$Reco_Combined, FUN = cumsum)  


Recodf_new15 <- read.csv("data/Recodf_new15.csv")
  
  
Recodf_new$ThresholdCum <- Recodf_new15$culModelled


plot(Recodf_new$date,Recodf_new$culMeasured,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Cumulative Reco", cex = 0.8)
lines(Recodf_new$date, Recodf_new$culMeanMod, type = "l", col = "red")
lines(Recodf_new$date, Recodf_new$culModelled, type = "l", col = "green")
lines(Recodf_new$date, Recodf_new$ThresholdCum, type = "l", col = "cyan")

legend(x = "topleft",
       legend = c("Measured Reco", "Mean model", "P-NP model", "15% model"),
       pch = c(1, 16, 16, 16),
       col = c("blue", "red", "green", "cyan"),
       lty = c(NA, 1,1,1),
       bty = "n")

### Calculate differences from the measured CUMILATIVE values Modelled - Measured  #########

Recodf_new$diffCum15 <- Recodf_new15$diffComb

Recodf_new$diffMean <- Recodf_new$culMeanMod - Recodf_new$culMeasured  
Recodf_new$diffComb <- Recodf_new$culModelled - Recodf_new$culMeasured 


plot(Recodf_new$date,Recodf_new$diffComb,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-200,50)
)
lines(Recodf_new$date, Recodf_new$diffMean, type = "l", col = "red")
lines(Recodf_new$date, Recodf_new$diffCum15, type = "l", col = "cyan")

legend(x = "bottomleft",
       legend = c("P-NP model difference", "Mean model difference", "15% model difference"),
       pch = c(1, 16, 16, 16),
       col = c("blue", "red", "cyan"),
       lty = c(NA, 1, 1, 1),
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


### Calculate differences from the measured values Measured - Modelled #########

Recodf_new$diff15Meas <- Recodf_new15$diffCombV
Recodf_new$Comb15 <- Recodf_new15$Reco_Combined


Recodf_new$diffMeanV <- Recodf_new$MeanM - Recodf_new$meanRECO 
Recodf_new$diffCombV <- Recodf_new$Reco_Combined - Recodf_new$meanRECO 


Recodf_new %>%
  na.omit() %>%
  ggplot(aes(x = meanRECO, y = MeanM))+
  geom_point(col = "red")+
  geom_point(aes(x = meanRECO, y=Reco_Combined), col = "blue")+
  geom_point(aes(x = meanRECO, y=Comb15), col = "cyan")+
  stat_regline_equation(aes(label = paste(..rr.label..)))+
  stat_smooth(method = "lm",formula = y ~ x ,size = 1)+
  ylab(~paste("Modelled Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab(~paste("Measured Reco, ", mu, "mol m"^-2,"s"^-1))+
  theme_classic()+
  ylim(0,4)
  #stat_cor(aes(label = after_stat(rr.label)), col = "black")




plot(Recodf_new$date,Recodf_new$diffCombV,  type = "l", col = "blue", xlab = "Year", 
     ylab =  "Difference from measured Reco", #cex = 0.8,
     ylim = c(-2,3)
)
lines(Recodf_new$date, Recodf_new$diffMeanV, type = "l", col = "red")
lines(Recodf_new$date, Recodf_new$diff15Meas, type = "l", col = "cyan")

legend(x = "topleft",
       legend = c( "P-NP model difference", "Mean model difference", "15% model difference"),
       pch = c(1, 16, 16, 16),
       col = c("blue", "red", "cyan"),
       lty = c(NA, 1, 1, 1),
       bty = "n")



Recodf_long <- Recodf_new %>%
  select(date, diffCombV, diffMeanV, diff15Meas) %>%
  pivot_longer(!date, names_to = "income", values_to = "count")

Recodf_long$date <- as.Date(Recodf_long$date)
Recodf_long$DOY <- yday (Recodf_long$date)

Recodf_long$Season = vector(mode = 'character', length = nrow(Recodf_long))
Recodf_long$Season[Recodf_long$DOY %in% c(1:59,305:366)] = 'Winter'
Recodf_long$Season[Recodf_long$DOY %in% 60:181] = 'Spring'
Recodf_long$Season[Recodf_long$DOY %in% 182:304] = 'Summer'


Recodf_long %>%
  filter(income == 'diffCombV') %>%
  ggplot(aes(x= date, y = count, fill = Season))+
  geom_col()+
  theme_bw()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Date")+
  ggtitle("Differences Measured VS Combined model")+
  ylim(-1,2)


errorComb <- Recodf_long %>%
  filter(income == 'diffCombV')

meanSprComb <- mean(errorComb$count[errorComb$Season == 'Spring'])
meanSumComb <- mean(errorComb$count[errorComb$Season == 'Summer'])
meanWinComb <- mean(errorComb$count[errorComb$Season == 'Winter'])

sumSprComb <- sum(errorComb$count[errorComb$Season == 'Spring'])
sumSumComb <- sum(errorComb$count[errorComb$Season == 'Summer'])
sumWinComb <- sum(errorComb$count[errorComb$Season == 'Winter'])


Recodf_long %>%
  filter(income == 'diffMeanV') %>%
  ggplot(aes(x= date, y = count, fill = Season))+
  geom_col()+
  theme_bw()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Date")+
  ggtitle("Differences Measured VS Mean model")+
  ylim(-1,2)

errorMean <- Recodf_long %>%
  filter(income == 'diffMeanV')

meanSprMean <- mean(errorMean$count[errorMean$Season == 'Spring'])
meanSumMean <- mean(errorMean$count[errorMean$Season == 'Summer'])
meanWinMean <- mean(errorMean$count[errorMean$Season == 'Winter'])

sumSprMean <- sum(errorMean$count[errorMean$Season == 'Spring'])
sumSumMean <- sum(errorMean$count[errorMean$Season == 'Summer'])
sumWinMean <- sum(errorMean$count[errorMean$Season == 'Winter'])


Recodf_long %>%
  filter(income == 'diff15Meas') %>%
  ggplot(aes(x= date, y = count, fill = Season))+
  geom_col()+
  theme_bw()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Date")+
  ggtitle("Differences Measured VS 15% model")+
  ylim(-1,2)

error15 <- Recodf_long %>%
  filter(income == 'diff15Meas')

meanSpr15 <- mean(error15$count[error15$Season == 'Spring'])
meanSum15 <- mean(error15$count[error15$Season == 'Summer'])
meanWin15 <- mean(error15$count[error15$Season == 'Winter'])

sumSpr15 <- sum(error15$count[error15$Season == 'Spring'])
sumSum15 <- sum(error15$count[error15$Season == 'Summer'])
sumWin15 <- sum(error15$count[error15$Season == 'Winter'])



################## Make all differ-s with modules ####################

Recodf_long$modules <- abs(Recodf_long$count)

Recodf_long %>%
  filter(income == 'diffCombV') %>%
  ggplot(aes(x= date, y = modules, fill = Season))+
  geom_col()+
  theme_bw()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Date")+
  ggtitle("Differences Measured VS Combined model")+
  ylim(0,2)


errorComb1 <- Recodf_long %>%
  filter(income == 'diffCombV')

meanSprComb1 <- mean(errorComb1$modules[errorComb1$Season == 'Spring'])
meanSumComb1 <- mean(errorComb1$modules[errorComb1$Season == 'Summer'])
meanWinComb1 <- mean(errorComb1$modules[errorComb1$Season == 'Winter'])

sumSprComb1 <- sum(errorComb1$modules[errorComb1$Season == 'Spring'])
sumSumComb1 <- sum(errorComb1$modules[errorComb1$Season == 'Summer'])
sumWinComb1 <- sum(errorComb1$modules[errorComb1$Season == 'Winter'])


Recodf_long %>%
  filter(income == 'diffMeanV') %>%
  ggplot(aes(x= date, y = modules, fill = Season))+
  geom_col()+
  theme_bw()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Date")+
  ggtitle("Differences Measured VS Mean model")+
  ylim(0,2)

errorMean1 <- Recodf_long %>%
  filter(income == 'diffMeanV')

meanSprMean1 <- mean(errorMean1$modules[errorMean1$Season == 'Spring'])
meanSumMean1 <- mean(errorMean1$modules[errorMean1$Season == 'Summer'])
meanWinMean1 <- mean(errorMean1$modules[errorMean1$Season == 'Winter'])

sumSprMean1 <- sum(errorMean1$modules[errorMean1$Season == 'Spring'])
sumSumMean1 <- sum(errorMean1$modules[errorMean1$Season == 'Summer'])
sumWinMean1 <- sum(errorMean1$modules[errorMean1$Season == 'Winter'])


Recodf_long %>%
  filter(income == 'diff15Meas') %>%
  ggplot(aes(x= date, y = modules, fill = Season))+
  geom_col()+
  theme_bw()+
  theme(text = element_text(size = 15))+
  ylab(~paste("Reco, ", mu, "mol m"^-2,"s"^-1))+
  xlab("Date")+
  ggtitle("Differences Measured VS 15% model")+
  ylim(0,2)

error15_1 <- Recodf_long %>%
  filter(income == 'diff15Meas')

meanSpr15_1 <- mean(error15_1$modules[error15_1$Season == 'Spring'])
meanSum15_1 <- mean(error15_1$modules[error15_1$Season == 'Summer'])
meanWin15_1 <- mean(error15_1$modules[error15_1$Season == 'Winter'])

sumSpr15_1 <- sum(error15_1$modules[error15_1$Season == 'Spring'])
sumSum15_1 <- sum(error15_1$modules[error15_1$Season == 'Summer'])
sumWin15_1 <- sum(error15_1$modules[error15_1$Season == 'Winter'])





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
 

# For combined MODEL ###########
loglik <- logLik(Param_model4_All)

n   <- attributes(loglik)$nobs 
p   <- attributes(loglik)$df 

dev <- -2*as.numeric(loglik)

my_AIC  <- dev + 2 * p
my_AIC1 <- dev + 2*4

my_AIC2 <- dev + 2*8

broom:: glance(Param_model4_All)

#  sigma isConv     finTol logLik   AIC   BIC deviance df.residual  nobs
#<dbl> <lgl>       <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#  1 0.356 TRUE   0.00000808 -1042. 2094. 2124.     342.        2696  2700

broom:: glance(Param_model4_NP)
# sigma isConv     finTol logLik   AIC   BIC deviance df.residual  nobs
#<dbl> <lgl>       <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#  1 0.198 TRUE   0.00000392   343. -677. -650.     66.3        1693  1697

broom:: glance(Param_model4_P)
# sigma isConv     finTol logLik   AIC   BIC deviance df.residual  nobs
#<dbl> <lgl>       <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#  1 0.435 TRUE   0.00000601  -587. 1184. 1208.     189.         999  1003



