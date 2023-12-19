# Author Dave Moore
# Create some plots from analysis so far
library(ggplot2)

USWkg12_20_summary  <- read.csv("data/USWkg12_20_summary.csv")
years_sum_Pulse0 <- read.csv("data/years_sum_Pulse0_DM.csv",  header = TRUE, na.strings = "NA")
years_sum_Pulse0$date = as.Date(years_sum_Pulse0$date)

years_sum_Pulse1<- read.csv("data/years_sum_Pulse1_DM.csv", header = TRUE, na.strings = "NA")
years_sum_Pulse1$date = as.Date(years_sum_Pulse1$date)

years_sum_Pulse0 <- na.omit(years_sum_Pulse0)
years_sum_Pulse1 <- na.omit(years_sum_Pulse1)
USWkg12_20_summary <- na.omit(USWkg12_20_summary)

  plot (years_sum_Pulse0$meanSWC15, years_sum_Pulse0$meanST15) 
  
  
  plot(years_sum_Pulse1$meanSWC15, years_sum_Pulse1$meanST15, 
       cex = years_sum_Pulse1$meanRECO/2, # set the point size based on meanRECO
       pch = 16, # use solid circles as the point symbol
       xlab = "Mean Soil Water Content (15 cm depth)", # add x-axis label
       ylab = "Mean Soil Temperature (15 cm depth)", # add y-axis label
       main = "SWC and Soil T - during pulses", # add plot title
       xlim=c(0,45))
  
  
    plot(years_sum_Pulse0$meanSWC15, years_sum_Pulse0$meanST15, 
       cex = years_sum_Pulse0$meanRECO/2, # set the point size based on meanRECO
       pch = 16, # use solid circles as the point symbol
       xlab = "Mean Soil Water Content (15 cm depth)", # add x-axis label
       ylab = "Mean Soil Temperature (15 cm depth)", # add y-axis label
       main = "SWC and Soil T - between pulses" , # add plot title
    xlim=c(0,45))
  
    plot(USWkg12_20_summary$meanSWC15, USWkg12_20_summary$meanST15, 
         cex = USWkg12_20_summary$meanRECO/2, # set the point size based on meanRECO
         pch = 16, # use solid circles as the point symbol
         xlab = "Mean Soil Water Content (15 cm depth)", # add x-axis label
         ylab = "Mean Soil Temperature (15 cm depth)", # add y-axis label
         main = "SWC and Soil T - all time") # add plot title
    
    

  sapply(years_sum_Pulse1, class)
  
  sum(is.na(years_sum_Pulse1$meanSWC15)) # count number of missing values in meanSWC15
  sum(is.na(years_sum_Pulse1$meanST15)) # count number of missing values in meanST15
  