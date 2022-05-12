############################################################################
###### Author: Dave Moore
###### Date: May 12 2022
###### Purpose: Read and explore merged Rainman data
############################################################################
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(tidyr)
library(ggplot2)

############################################################################
# Read in files from data store
############################################################################


############################################################################
#### Merged file is created by 001_RM_Read_Merge_Flux_Rain_VWC.R
#### on May 12 - Dave has some concerns that the merged VWC contains
#### multiple probes per Date10 - need to check with Fangue on protocol
### reommend pausing Rainman analysis and focusing on Kendall
############################################################################

flux_RM_VWC_precip <- read.csv("data/RM_dailyFlux_VWC_Rain.csv",
                        sep=",",header=TRUE, na.strings = "NA")
flux_RM_VWC_precip$Date10 = as.Date(flux_RM_VWC_precip$Date10)

####### Plots #######
plot(flux_RM_VWC_precip$Date10, flux_RM_VWC_precip$Fsoil_daily)

# PlotFlux by Treatment in RAINMAN  
Flux_byTreat_mge <- ggplot(flux_RM_VWC_precip, 
                           aes(x=Date10, y=Fsoil_daily, 
                               group=Summer, 
                               color=Summer
                           )) +
  theme_bw() +
  theme(axis.ticks.length = unit(-0.2, "cm"))+
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  xlab("Date") +
  ylab("Soil Efflux (umol m-2 s-1)") +
  geom_point(size = 2) 


# Plot VWC by Treatment in RAINMAN  
VWC_daily_byTreat_mge <- ggplot(flux_RM_VWC_precip, 
                                aes(x=Date10, y=VWC_daily,
                                    group=Summer, 
                                    color=Summer
                                )) +
  theme_bw() +
  theme(axis.ticks.length = unit(-0.2, "cm"))+
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  xlab("Date") +
  ylab("Volumetric Water Content (prop)") +
  geom_point(size = 2) 



# Plot Precip by Treatment in RAINMAN  
RAIN_daily_byTreat_mge <- ggplot(flux_RM_VWC_precip, 
                                 aes(x=Date10, y=Precip_daily,
                                     group=Summer, 
                                     color=Summer
                                 )) +
  theme_bw() +
  theme(axis.ticks.length = unit(-0.2, "cm"))+
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  xlab("Date") +
  ylab("Precip (mm)") +
  geom_point(size = 2) 


# Plot Flux by VWC in RAINMAN  
Flux_VWC_daily_byTreat_mge <- ggplot(flux_RM_VWC_precip, 
                                     aes(x=VWC_daily, y=,Fsoil_daily,
                                         group=Summer, 
                                         color=Summer
                                     )) +
  theme_bw() +
  theme(axis.ticks.length = unit(-0.2, "cm"))+
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  
  #scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  xlab("Volumetric Water Content (prop)") +
  ylab("Soil Efflux (umol m-2 s-1)") +
  geom_point(size = 2) 


### display plots
Flux_byTreat_mge
VWC_daily_byTreat_mge
RAIN_daily_byTreat_mge
Flux_VWC_daily_byTreat_mge

###### Calculate main effects
flux_RM_VWC_precip_BYTRT =flux_RM_VWC_precip %>%
  group_by(House) %>% 
  summarize(
    Mean_Fsoil_TRT = mean(Fsoil_daily, na.rm=TRUE),
    Mean_VWC_TRT = mean(VWC_daily, na.rm=TRUE),
    Mean_Precip_TRT = mean(Precip_daily, na.rm=TRUE),
  )
flux_RM_VWC_precip_BYTRT
