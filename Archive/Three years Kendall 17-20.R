# 19-10-2022
# Anastasia Makhnykina

#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)
library(PerformanceAnalytics)
library(xtable)
library(ggpubr)
library(ggplot2)
library(colorRamps)
library(reshape2)
library(zoo)
library(deSolve)
library(stats)

####### Open Summary files
summary2017_new <- read.csv("data/summary2017_new.csv", 
                            header=TRUE, na.strings = "NaN")
summary2018_new <- read.csv("data/summary2018_new.csv", 
                            header=TRUE, na.strings = "NaN")
summary2019_new <-read.csv("data/summary2019_new.csv", 
                           header=TRUE, na.strings = "NaN")
summary2020_new <- read.csv("data/summary2020_new.csv", 
                            header=TRUE, na.strings = "NaN")
  

#### cat - put together 3 years

years_sum1 <- rbind(summary2017_new,  summary2018_new, summary2019_new, summary2020_new)

years_sum1$Pulse_DOY <- as.numeric(as.character(years_sum1$Pulse_DOY, na.rm = TRUE))
years_sum1[is.na(years_sum1)] <- 0

years_sum_Pulse0 <- years_sum1 %>%
  filter(Pulse_DOY == 0)

write.csv(years_sum_Pulse0, "data/years_sum_Pulse0.csv")

years_sum_Pulse1 <- years_sum1 %>%
  filter(Pulse_DOY > 0)

write.csv(years_sum_Pulse1, "data/years_sum_Pulse1.csv")

write.csv(years_sum1, "data/years_sum1.csv")

########## Temper VS Moist Space for all 4 years ########################


years_sum_Pulse0$meanSWC5 <- as.numeric(as.character(years_sum_Pulse0$meanSWC5))

years_sum_Pulse0 %>%
  #filter(RainEvent==1)%>%
  ggplot(aes(x= meanSWC5, y= meanST5, size = meanRECO, color = Season)) + 
  geom_point()

years_sum_Pulse0 %>%
  #filter(RainEvent==1)%>%
  ggplot(aes(x= meanGPP, y= meanRECO, #size = meanSWC5, #color = Season
             )) + 
  geom_point(size = 3, shape = 1
    )+
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  xlab('Mean GPP')+
  ylab('Mean Reco')+
  ggtitle('Non-Pulse time')



years_sum_Pulse1 %>%
  #filter(RainEvent==1)%>%
  ggplot(aes(x= meanSWC5, y= meanST5, size = meanRECO, color = Season)) + 
  geom_point()

years_sum_Pulse1 %>%
  #filter(RainEvent==1)%>%
  ggplot(aes(x= meanGPP, y= meanRECO, #size = meanSWC5, #color = Season
  )) + 
  geom_point(size = 3, shape = 1
  )+
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(text = element_text(size = 15))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  xlab('Mean GPP')+
  ylab('Mean Reco')+
  ggtitle('Pulse time')

########## Non-linear model for daily data ##################
# Fitting parameters for model4 from Roby et al 2019

Param_model4 <- nls(meanRECO ~ Fref*((meanGPP/GPPmax +n)/1+n) *(1-c4*(0.1-meanSWC5)^2)*exp(b4*meanST5), 
                    data = years_sum_Pulse0,
                    start = list(Fref=0.75,  c4=56.54, b4=0.04, n=0.84),
                    control = nls.control(maxiter = 1000, minFactor = 0.01)
)

Summary_Model4 = summary(Param_model4)


# Parameters:(Summary_Model4)
#   Estimate Std. Error t value Pr(>|t|)    
# Fref  1.061e+00  6.586e-02  16.112  < 2e-16 ***
#   c4   -4.974e-04  9.878e-05  -5.035 5.54e-07 ***
#   b4    3.672e-02  1.885e-03  19.476  < 2e-16 ***
#   n     6.667e-02  3.318e-03  20.096  < 2e-16 ***

Fref = 1.061e+00 #0.75
SMopt =0.125 #Note that we FIXED SMopt at the value used in Roby et al 2019
c4 = -4.974e-04  #56.54
b4 =  3.672e-02 #0.04
n=  6.667e-02  #0.84

years_sum_Pulse0$SoilMoisture =years_sum_Pulse0$meanSWC5/100
meanSWC5 = years_sum_Pulse0$SoilMoisture

meanST5 = years_sum_Pulse0$meanST5
meanGPP = years_sum_Pulse0$meanGPP
GPPmax = max(years_sum_Pulse0$meanGPP)

model4 = Fref*((meanGPP/GPPmax +n)/1+n) *(1-c4*(SMopt-meanSWC5)^2)*exp(b4*meanST5)

plot(model4)


# Plot the RECO time series
plot(years_sum_Pulse0$meanRECO, type = "p", col = "blue", xlab = "Timestamp", ylab = "RECO")
# Add the model output time series to the plot
lines( model4, type = "l", col = "red")


plot(years_sum_Pulse0$meanRECO,model4, xlab = "Observation", ylab = "Model4")

# ADD CODE TO ESTIMATE THE Pulse time nls

model4_P <- nls(meanRECO ~ Fref*((meanGPP/GPPmax +n)/1+n) *(1-c4*(0.1-meanSWC5)^2)*exp(b4*meanST5), 
                data = years_sum_Pulse1,
                start = list(Fref=0.75,  c4=56.54, b4=0.04, n=0.84),
                control = nls.control(maxiter = 1000, minFactor = 0.01)
)

Summary_Model4_P = summary(model4_P)

Summary_Model4_P



# Pulse parameters
FrefP = 5.920e-01 
SMoptP =0.125 
c4P = -1.724e-04  
b4P =  4.376e-02
nP=  3.001e-01


# SET UP MODEL FOR years_sum1

# ########################################################################## #
#                                                                            #
#       Calculate the modeled Respiration for the full timeseries            #
#                                                                            #
# ########################################################################## #

Fref = 1.061e+00 #0.75
SMopt =0.125 #Note that we FIXED SMopt at the value used in Roby et al 2019
c4 = -4.974e-04  #56.54
b4 =  3.672e-02 #0.04
n=  6.667e-02  #0.84

# Setting up drivers for all time
All_meanSWC5 =years_sum1$meanSWC5/100
All_meanST5 = years_sum1$meanST5
All_meanGPP = years_sum1$meanGPP
All_GPPmax = max(years_sum1$meanGPP)

#run model for full time series based on non-pulse time parameters
ALL_model4 = Fref*((All_meanGPP/All_GPPmax +n)/1+n) *(1-c4*(SMopt-All_meanSWC5)^2)*exp(b4*All_meanST5)
#run model for full time series based on pulse time parameters
All_model4_P = FrefP*((All_meanGPP/All_GPPmax +nP)/1+nP) *(1-c4P*(SMoptP-All_meanSWC5)^2)*exp(b4P*All_meanST5)


# Plot the RECO time series
plot(years_sum1$meanRECO, type = "p", col = "blue", xlab = "Timestamp", ylab = "RECO")
# Add the model output time series to the plot
lines( ALL_model4, type = "l", col = "red")
lines( All_model4_P, type = "l", col = "cyan")
# create the legend
legend(x = "topright",
       legend = c("Measured RECO", "Pulse Model", "Non-Pulse Model"),
       pch = c(1, NA, NA),
       col = c("blue", "red", "cyan"),
       lty = c(NA, 1, 1),
       bty = "n")


plot(years_sum1$meanRECO,ALL_model4, type = "p", col = "red", xlab = "MEASURED RECO", ylab = "Non-pulse model RECO")
text(x = 1, y = 2.5, labels = "Over estimation")
text(x = 3, y = 0.5, labels = "Under estimation")
points( years_sum1$meanRECO,years_sum1$meanRECO,  type = "l", col = "red")

plot(years_sum1$meanRECO,All_model4_P,  type = "p", col = "cyan", xlab = "MEASURED RECO", ylab = "Pulse model RECO")
text(x = 1, y = 2.5, labels = "Over estimation")
text(x = 3, y = 0.5, labels = "Under estimation")
points( years_sum1$meanRECO,years_sum1$meanRECO,  type = "l", col = "cyan")


# Calulate the model residual and investigate whether residuals are higher during pulse times. 
Model_residual = years_sum1$meanRECO-ALL_model4
plot(years_sum1$meanSWC5,Model_residual, xlab = "SWC", ylab = "Model Error")

