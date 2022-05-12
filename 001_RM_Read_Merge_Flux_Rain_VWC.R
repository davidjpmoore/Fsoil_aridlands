############################################################################
###### Author: Dave Moore
###### Date: May 11 2022
###### Purpose: Read in irrigation data from RAINMAN & 
###### organize by treatment and date


############################################################################
# Read in files from data store
############################################################################


############################################################################
#### freq dataset reports the rainfall received by each irrigation treatment
############################################################################
freqrainman <- read.csv("data/Complete Irrigation.csv",
                        sep=";",header=TRUE, na.strings = ".")
freqrainman$Date10 <- as.Date(substr(freqrainman$Date, 1,10))
#formating the 10 digit date as a DATE
#create DOY from date field
freqrainman$DOY <- as.numeric(paste(yday(freqrainman$Date)))
#need to document this file (where does it come from?)

#using pivot_longer to change columns to rows
freqrainman_slim <-
  pivot_longer(data= freqrainman,
               cols = starts_with("S"),
               names_to = "Summer_mm",
               values_to = "Precip"
  )
#add Summer to be consistent with other datasets
freqrainman_slim$Summer <- as.character(substr(freqrainman_slim$Summer_mm, 1,2))
freqrainman_slim$Precip[is.na(freqrainman_slim$Precip)] <- 0
freqrainman_slim <- subset(freqrainman_slim, select=-c(Summer_mm, Date, DOY))


Precip_daily =freqrainman_slim %>%
  group_by(Date10, Summer) %>% 
  summarize(
    Precip_daily = sum(Precip,na.rm=TRUE) )
# 
# plot(Precip_daily$Date10, Precip_daily$Precip_daily)
# plot(freqrainman_slim$Date10,freqrainman_slim$Precip)
# plot(freqrainman$Date10,freqrainman$S4.mm.)
# plot(freqrainman$S1.mm.)
# plot(freqrainman$S2.mm.)
# plot(freqrainman$S3.mm.)

# Plot the Rain by Summer Treatment in RAINMAN  
Rain_byTreat <- ggplot(Precip_daily, 
                       aes(x=Date10, y=Precip_daily, 
                           group=Summer, shape=Summer, color=Summer
                       )) +
  theme_bw() +
  theme(axis.ticks.length = unit(-0.2, "cm"))+
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  xlab("Date") +
  ylab("Irrigation (mm)") +
  geom_point(size = 2) 

####Display the plot
Rain_byTreat



####################
### export file ###
####################
# Please us the convention that
# filenames that start with RM = Rainman 
# filenames that start with WGK = WalnutGultchKendall

write_csv(Precip_daily, file = "data/RM_daily_precip_by_S.csv")

############################################################################
#
# Soil Efflux data sent from Jake
#
############################################################################

flux_rainman <- read.csv("data/AllColumns_FirstCleanMethod.csv",
                         sep=",",header=TRUE, na.strings = "NaN")
flux_rainman$Date10 <- as.Date(substr(flux_rainman$Date, 1,10))
#formating the 10 digit date as a DATE
flux_rainman$DOY <- as.numeric(paste(yday(flux_rainman$Date10)))
#need to document this file (where does it come from? email from Jake?)

flux_RM <-  flux_rainman %>% 
  mutate(Plot = recode(Plot, '1' = '01', '2' = '02', 
                       '3' = '03', '4' = '04', '5' = '05','6' = '06', 
                       '7' = '07', '8' = '08', '9' = '09', 
                       '10' = '10', '11' = '11', '12' = '12'))  %>%
  mutate(House = recode(House, '1' = '01', '2' = '02', 
                        '3' = '03', '4' = '04', '5' = '05')) %>%
  rename(Summer=S, Winter=W)

flux_RM$Sum_Wint_TRT = paste(flux_RM$Winter, flux_RM$Summer, sep="_")
flux_RM$Plot = as.character(flux_RM$Plot)
flux_RM$House = as.character(flux_RM$House)
flux_RM$House_plot = paste(flux_RM$House, flux_RM$Plot, sep="_")
#columns above created for a join check - join check passed
#removing columns to avoid duplications
flux_RM <- subset(flux_RM, select = -c(Winter, Summer, Sum_Wint_TRT))



# Plot the Flux by House_Plot  in RAINMAN  
Flux_byTreat <- ggplot(flux_RM, 
                       aes(x=Date10, y=Flux, 
                           group=House_plot, 
                           color=House_plot
                       )) +
  theme_bw() +
  theme(axis.ticks.length = unit(-0.2, "cm"))+
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  xlab("Date") +
  ylab("Soil Efflux (umol m-2 s-1)") +
  geom_point(size = 2) 

####Display the plot
Flux_byTreat


###############################################################
# read treatment codes from RAINMAN DATA STORE
# https://docs.google.com/spreadsheets/d/1eL_CKVklv2upHVd3Hck9stpddoQscd22yFScTaqrdxM/edit#gid=853864262
# This file was downloaded to a CSV file Path= "~/data/TreatmentCODED.csv"
###############################################################

TRT_RM <- read.csv("data/TreatmentCODED.csv",
                   sep=",",header=TRUE, na.strings = "NA")
TRT_RM <-  TRT_RM %>% 
  mutate(Plot = recode(Plot, '1' = '01', '2' = '02', 
                       '3' = '03', '4' = '04', '5' = '05','6' = '06', 
                       '7' = '07', '8' = '08', '9' = '09', 
                       '10' = '10', '11' = '11', '12' = '12'))  %>%
  mutate(House = recode(House, '1' = '01', '2' = '02', 
                        '3' = '03', '4' = '04', '5' = '05')) 

TRT_RM$Sum_Wint_TRT = paste(TRT_RM$Winter, TRT_RM$Summer, sep="_")
TRT_RM$Plot = as.character(TRT_RM$Plot)
TRT_RM$House = as.character(TRT_RM$House)
TRT_RM$House_plot = paste(TRT_RM$House, TRT_RM$Plot, sep="_")
#slim down TRT 
TRT_RM_mge=subset(TRT_RM, select = c(House_plot, Winter, Summer, Sum_Wint_TRT))
write_csv(TRT_RM_mge, file="data/TreatmentKey_tidy_RM.csv")
###############################################################


#Join with Treatment information for check
#Note there are more treatment combinations than VWC probes
flux_RM_jn =  full_join( TRT_RM_mge,flux_RM,
                         by = c("House_plot" = "House_plot"))
flux_RM = subset(flux_RM_jn, 
                 select = c(Date10 ,DOY, House, Plot, Winter, Summer, 
                            Sum_Wint_TRT, House_plot,  Tchamb., Flux, R2))


flux_RM %>%
  #  mutate(Daily = day(Date10)) %>%
  group_by(Date10, House_plot, na.rm=TRUE) 

plot( flux_RM$Precip, na.rm=TRUE)

flux_RM_daily =flux_RM %>%
  group_by(Date10, House_plot, Summer) %>% 
  summarize(
    Fsoil_daily = sum(Flux, na.rm=TRUE),
    Tcham_daily = mean(Tchamb., na.rm=TRUE) )


# PlotFlux by Treatment in RAINMAN  
Flux_byTreat <- ggplot(flux_RM_daily, 
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

###display plot
Flux_byTreat

#### are multiple probes still in this summary by day?


#  ########################################################### 
# # READ DAILY VWC data unless already created
# # file="data/VWC_daily.csv"
#  ###########################################################
VWC_slim_RM_daily <- read.csv("data/RM_VWC_slim_byTRT_daily.csv",
                              sep=",",header=TRUE, na.strings = "NA")
VWC_slim_RM_daily$Date10 = as.Date(VWC_slim_RM_daily$Date10)
#  ###########################################################

Precip_daily
flux_RM =  full_join(flux_RM,freqrainman_slim,  
                     by = c("Date10" = "Date10", "Summer" = "Summer"))
#  ###########################################################
#  Merge daily rainman flux with VWC
#  ###########################################################
#VWC_slim
flux_RM_VWC =full_join( flux_RM_daily,VWC_slim_RM_daily, by = c("House_plot" = "House_plot", "Date10" = "Date10", "Summer"="Summer"))
flux_RM_VWC_precip =full_join( flux_RM_VWC,Precip_daily, by = c("Date10" = "Date10", "Summer"="Summer"))


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


# PlotFlux by Treatment in RAINMAN  
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




# PlotFlux by Treatment in RAINMAN  
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


# PlotFlux by VWC in RAINMAN  
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




###display plot
Flux_byTreat
Flux_byTreat_mge
VWC_daily_byTreat_mge
Flux_VWC_daily_byTreat_mge


write_csv(flux_RM_VWC_precip, file="data/RM_dailyFlux_VWC_Rain.csv")


  
