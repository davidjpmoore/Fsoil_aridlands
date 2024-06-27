# 27-06-2024
# Anastasia Makhnykina

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

# In this file we will plot figures to show interactions between Rsoil and GPP

# Open file we need

summary_Cham_P <- read.csv("data/All summary chamber.csv")
Pulse_Cham <- read.csv("data/Pulse sum chamber.csv")
NonPulse_Cham <- read.csv("data/NonPulse sum chamber.csv")

Pulse_Cham$SWC_per <- Pulse_Cham$meanSWC*100

hist(Pulse_Cham$SWC_per)

# Plot the same for the mean ST / SWC
plot(Pulse_Cham$SWC_per, Pulse_Cham$meanTsoil, 
     cex = Pulse_Cham$meanRsoil/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC (5 cm depth)", # add x-axis label
     ylab = "Mean soil temperature (5 cm depth)", # add y-axis label
     main = "SWC and Soil T - during pulses", # add plot title
     xlim=c(0,100))


NonPulse_Cham$SWC_per <- NonPulse_Cham$meanSWC*100

plot(NonPulse_Cham$SWC_per, NonPulse_Cham$meanTsoil, 
     cex = NonPulse_Cham$meanRsoil/2, # set the point size based on meanRECO
     pch = 16, # use solid circles as the point symbol
     xlab = "Mean SWC (5 cm depth)", # add x-axis label
     ylab = "Mean soil temperature (5 cm depth)", # add y-axis label
     main = "SWC and Soil T - between pulses" , # add plot title
     xlim=c(0,100))



