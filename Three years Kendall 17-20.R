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
library(Hmisc)
library(ggpubr)
library(ggplot2)
library(colorRamps)
library(reshape2)
library(zoo)
library(deSolve)
library(stats)


#### cat - put together 3 years

years_sum1 <- rbind(summary2017_new,  summary2018_new, summary2019_new, summary2020_new)

years_sum1[is.na(years_sum1)] = 0

years_sum_Pulse0 <- years_sum1 %>%
  filter(Pulse_DOY == 0)

years_sum_Pulse1 <- years_sum1 %>%
  filter(Pulse_DOY > 0)

########## Non-linear model for daily data ##################

Fref = 0.75
SWCopt = 0.25

model4 <- nls(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = years_sum1,
              start = list(SWCopt = 0.25, Fref=0.75),
              control = list(tolerance =100)
)

summary(model4)
meanSWC30 =  years_sum1$meanSWC30
meanST30 =  years_sum1$meanST30
meanGPP =  years_sum1$meanGPP
Fref = -4.397e-16
SWCopt = 3.475e+01

RECOmod4 <- Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP
plot(RECOmod4)
plot(years_sum1$meanRECO)


cor(RECOmod4,years_sum1$meanRECO)

plot(years_sum1$meanRECO [RECOmod4>1], RECOmod4 [RECOmod4>1])


########### For Non-pulse time

Fref = 0.75
SWCopt = 0.25
model5 <- nls(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = years_sum_Pulse0,
              start = list(SWCopt = 0.25, Fref=0.75)
)
library(bbmle)
logLik(model5)
AIC(model5)
BIC (model5)

summary(model5)
meanSWC30 =  years_sum_Pulse0$meanSWC30
meanST30 =  years_sum_Pulse0$meanST30
meanGPP =  years_sum_Pulse0$meanGPP
Fref = -1.089e-15
SWCopt = 2.372e+01



RECOmod5 <- Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP
plot(RECOmod5)


cor(RECOmod5,years_sum_Pulse0$meanRECO)

plot(years_sum_Pulse0$meanRECO [RECOmod5>1], RECOmod5 [RECOmod5>1])



############ Something happened with Predict
lines(years_sum1$meanRECO,predict(RECOmod4), lty=2,col="red",lwd=3)


########### For Pulse time

Fref = 0.75
SWCopt = 0.25
model6 <- nls(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = years_sum_Pulse1,
              start = list(SWCopt = 0.25, Fref=0.75)
)

summary(model6)
meanSWC30 =  years_sum_Pulse1$meanSWC30
meanST30 =  years_sum_Pulse1$meanST30
meanGPP =  years_sum_Pulse1$meanGPP
Fref = -2.585e-15
SWCopt = 1.827e+01

RECOmod6 <- Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP
plot(RECOmod6)


cor(RECOmod6,years_sum_Pulse1$meanRECO)

plot(RECOmod6,years_sum_Pulse1$meanRECO[years_sum_Pulse1$meanRECO>0])

########### How to make graph with lines
# plot(dat2$year, dat2$n, type = "l", col = "blue",
# main = "Women born with different names",
# xlab = "Year",
# ylab = "Number of babies born")
# lines(dat$year, dat$n, col = "red")
# lines(dat3$year, dat3$n, col = "orange")


### For NON Pulse time

plot(years_sum_Pulse0$meanRECO [RECOmod5>1], RECOmod5 [RECOmod5>1], type = "p", col = "blue",
main = "Non-Pulse time",
xlab = "Measured",
ylab = "Modelled")
# lines(dat$year, dat$n, col = "red")
# lines(dat3$year, dat3$n, col = "orange")

plot(years_sum1$DOY+(years_sum1$YEAR-2017)*365, RECOmod4, type = "l", col = "blue",
     main = "SWC-Temp-GPP Model",
     xlab = "Data",
     ylab = "Fluxes")



lines(years_sum_Pulse0$DOY, RECOmod5, col = "red")
lines(years_sum1$DOY, years_sum1$meanRECO, col = "orange")

legend("topright", c("Modelled All", "Modelled Non-Pulse", "Measured"),
       lty = c(1,1,1),
       col = c("blue", "red", "orange"))





