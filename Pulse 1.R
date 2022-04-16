#4/13/2022
#Anastasia Makhnykina
#grab data we need


#Load Datarain
datarain_pro=read.csv("data/datarain_processed.csv", header=TRUE, na.strings = "NaN")
#remove unnessessary columns


Pulse1=read.csv("data/Pulse1.csv", header=TRUE, na.strings = "NaN")
#remove unnessessary columns


#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)


# Required package for quick package downloading and loading 
#install.packages("pacman")

# Downloads and load required packages
pacman::p_load(dlookr,
               formattable,
               ggdist,
               ggpubr,
               ggridges,
               kableExtra,
               knitr,
               papeR,
               RColorBrewer,
               Stat2Data,
               tidyverse)

# DATA ANALYSIS
formattable((head(Pulse1)))

#Describing our data

Pulse1 %>%
  diagnose(RECO)

formattable(diagnose_numeric(Pulse1))
summary(Pulse1)

summary(Pulse1$AT2)
summary(Pulse1$r)
summary(Pulse1$RECO)

hist.default(Pulse1$AT2, col='gray')

hist.default(Pulse1$NEE, col='gray')

hist.default(Pulse1$r, col='gray')

hist.default(Pulse1$RECO, col='gray')
table(Pulse1$RECO)

plot(Pulse1$GPP~Pulse1$RECO, col='blue')

# T-test shows the statistical inferences and the confidence interval .as outcomes.
# The p-value is the probability value significant to the null hypothesis. 
# And the percentage value is the confidence interval.

t.test(Pulse1$RECO)

var(Pulse1$RECO)

plot(Pulse1$data_time_Start,Pulse1$RECO, col='blue')

ggplot(mapping=aes(x=Pulse1$dateStart,y=Pulse1$RECO))+
  geom_point()

ggplot(mapping=aes(x=Pulse1$NEE,y=Pulse1$RECO))+
  geom_point()

ggplot(mapping=aes(x=Pulse1$NEE,y=Pulse1$RECO))+
  geom_point()

ggplot(Pulse1, aes(x=Pulse1$DOY_S))+
  geom_point(aes(y=Pulse1$RECO, color="red"))+
  geom_point(aes(y=Pulse1$SWC5, color="blue"))



ggplot(Pulse1, aes(x=Pulse1$DOY_S, Pulse1$RECO))+
  geom_point(aes(y=Pulse1$RECO, color="red"))+
  geom_point(aes(y=Pulse1$SWC5, color="blue"))+
  scale_y_continuous(
    name="Reco (micromol CO2 m-2 s-1)",
    sec.axis=sec_axis(~., name="SWC 5 cm (%)")
  )

#Some statistical analysis for the numeric data

head(Pulse1)

numeric_Pulse1 <- diagnose_numeric(Pulse1)


#Making the correlation matrix 

cormatrix <- cor(Pulse1[sapply(Pulse1, is.numeric)])

corrplot(cormatrix)


#1 - Choose the columns which I really need for analysis
#2 - Make the analysis of rain events based on the previous conditions and look at the variables directions after the rain OR pulse
#3 - Which rain can be qualified as pulse??? 


Pulse1_pro <- select(Pulse1, c(PA, AT2, RH2, AT6, RH6, r, SWC5, 
                               SWC15, SWC30, SWC30, ST5, 
                               ST15,ST30, NEE, RECO, GPP, high_precip, 
                               RainEvent, dateStart, dateEnd, data_time_Start,
                               data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain))


plot(Pulse1_pro$SWC5)

#Make this graph without 0-values
plot(Pulse1_pro$r) 









