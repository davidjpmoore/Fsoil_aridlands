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
              start = list(SWCopt = 0.25, Fref=0.75)
)


model4 <- nls(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = years_sum1,
              start = list(SWCopt = 0.25, Fref=0.75),
              control = nls.control(maxiter = 100)
)


summary(model4)
meanSWC30 =  years_sum1$meanSWC30
meanST30 =  years_sum1$meanST30
meanGPP =  years_sum1$meanGPP
Fref = -1.119e-15
SWCopt = 2.385e+01

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


logLik(model5)
AIC(model5)
BIC (model5)

summary(model5)
confint(model5)
vcov(model5)


############ Maximum likelihood estimation ##########################
library(bbmle)
funmod5 = function(a,b,sigma) {
  Y.pred= a*(1-(years_sum_Pulse0$meanSWC30-b)^2) * exp(years_sum_Pulse0$meanST30) * years_sum_Pulse0$meanGPP
  -sum(dnorm(y1, mean=Y.pred, sd= sigma, log=TRUE))
}
y1=years_sum_Pulse0$meanRECO
mle2.model <- mle2(funmod5,
                   start = list(a=0.75, b = 0.25, sigma = 1))

warnings()
summary(mle2.model)
Y.pred = -1.3397e-16*(1-(years_sum_Pulse0$meanSWC30-5.8750e+01)^2) * exp(years_sum_Pulse0$meanST30) * years_sum_Pulse0$meanGPP
plot(Y.pred)


-logLik(mle2.model)
deviance(mle2.model)
confint(mle2.model)
profile.mle2.model <- profile(mle2.model)
confint(profile.mle2.model)
confint(model5)

par(mfrow=c(1,3))
plot(profile.mle2.model, abs = T, conf = c (99,95,90,80,50)/100)

vcov(mle2.model)
sqrt(vcov(mle2.model)[1,1])
############ Probably here we also have a mistake because AIC of mle2.model and model5 should be the same
AIC(mle2.model)
AIC(model5)



meanSWC30 =  years_sum_Pulse0$meanSWC30
meanST30 =  years_sum_Pulse0$meanST30
meanGPP =  years_sum_Pulse0$meanGPP
Fref = -1.089e-15
SWCopt = 2.372e+01


RECOmod5 <- Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP
plot(RECOmod5)


cor(RECOmod5,years_sum_Pulse0$meanRECO)
cor(Y.pred, years_sum_Pulse0$meanRECO)


plot(years_sum_Pulse0$meanRECO [RECOmod5>1], RECOmod5 [RECOmod5>1])
plot(years_sum_Pulse0$meanRECO [Y.pred>1], Y.pred [Y.pred>1])


############ Something happened with Predict
lines(years_sum1$meanRECO,predict(RECOmod4), lty=2,col="red",lwd=3)


########### For Pulse time

Fref = 0.75
SWCopt = 0.25
model6 <- nls(meanRECO ~ Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP, 
              data = years_sum_Pulse1,
              start = list(SWCopt = 0.25, Fref=0.75)
)

logLik(model6)
AIC(model6)
BIC (model6)

summary(model6)
confint(model6)
vcov(model6)

funmod6 = function(a,b,sigma) {
  Y.pred1= a*(1-(years_sum_Pulse1$meanSWC30-b)^2) * exp(years_sum_Pulse1$meanST30) * years_sum_Pulse1$meanGPP
  -sum(dnorm(y1, mean=Y.pred1, sd= sigma, log=TRUE))
}
y1=years_sum_Pulse1$meanRECO
mle3.model <- mle2(funmod6,
                   start = list(a=0.75, b = 0.25, sigma = 1))

warnings()
summary(mle3.model)
Y.pred1 = 8.1110e-16*(1-(years_sum_Pulse1$meanSWC30-1.5838e+01)^2) * exp(years_sum_Pulse1$meanST30) * years_sum_Pulse1$meanGPP
plot(Y.pred1)


-logLik(mle3.model)
deviance(mle3.model)
confint(mle3.model)
profile.mle3.model <- profile(mle3.model)
confint(profile.mle3.model)
confint(model6)

par(mfrow=c(1,3))
plot(profile.mle3.model, abs = T, conf = c (99,95,90,80,50)/100)

vcov(mle3.model)
sqrt(vcov(mle3.model)[1,1])
############ Probably here we also have a mistake because AIC of mle2.model and model5 should be the same
AIC(mle3.model)
AIC(model6)


meanSWC30 =  years_sum_Pulse1$meanSWC30
meanST30 =  years_sum_Pulse1$meanST30
meanGPP =  years_sum_Pulse1$meanGPP
Fref = -2.585e-15
SWCopt = 1.827e+01

RECOmod6 <- Fref*(1-(meanSWC30-SWCopt)^2) * exp(meanST30) * meanGPP
plot(RECOmod6)

cor(RECOmod6,years_sum_Pulse1$meanRECO)
cor(Y.pred1, years_sum_Pulse1$meanRECO)


plot(years_sum_Pulse1$meanRECO [RECOmod6>1], RECOmod6 [RECOmod6>1])
plot(years_sum_Pulse1$meanRECO [Y.pred>1], Y.pred [Y.pred>1])










########### How to make graph with lines
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


#### mle2

plot(years_sum_Pulse0$meanRECO [Y.pred>1], Y.pred [Y.pred>1], type = "p", col = "blue",
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




################### Night time MODELS ######################################






