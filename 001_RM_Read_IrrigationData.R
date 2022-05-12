############################################################################
###### Author: Dave Moore
###### Date: May 11 2022
###### Purpose: Read in irrigation data from RAINMAN & 
###### organize by treatment and date
############################################################################
#### freq dataset reports the rainfall recieved by each irrigation treatment
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


RMPrecip_daily =freqrainman_slim %>%
  group_by(Date10, Summer) %>% 
  summarize(
    RMPrecip_daily = sum(Precip,na.rm=TRUE) )

plot(RMPrecip_daily$Date10, RMPrecip_daily$Precip_daily)

plot(freqrainman_slim$Date10,freqrainman_slim$Precip)
plot(freqrainman$Date10,freqrainman$S4.mm.)
plot(freqrainman$S1.mm.)
plot(freqrainman$S2.mm.)
plot(freqrainman$S3.mm.)


# Plot the Rain by Summer Treatment in RAINMAN  
Rain_byTreat <- ggplot(RMPrecip_daily, 
                       aes(x=Date10, y=RMPrecip_daily, 
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
 
####Diplay the plot
  Rain_byTreat
  
####################
### explort file ###
####################
  # Please us the convention that
  # filenames that start with RM = Rainman 
  # filenames that start with WGK = WalnutGultchKendall
  write_csv(RMPrecip_daily, file = "data/RM_daily_precip_by_S.csv")
  
