#4/12/2022
#Anastasia Makhnykina

#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(tidyr)
library(ggplot2)

install.packages("pacman")

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

#open cvs
datarain=read.csv("RainMan2022/Fsoil_aridlands/data/20162017Meteo.csv", header=TRUE, na.strings = "NaN")


#Date of Start and End of Measurements 
datarain$year=substr(datarain$TIMESTAMP_START,1,4)
datarain$month=substr(datarain$TIMESTAMP_START, 5,6)
datarain$day=substr(datarain$TIMESTAMP_START, 7,8)
datarain$hour=substr(datarain$TIMESTAMP_START,9,10)
datarain$min=substr(datarain$TIMESTAMP_START,11,12)

datarain$year1=substr(datarain$TIMESTAMP_END,1,4)
datarain$month1=substr(datarain$TIMESTAMP_END, 5,6)
datarain$day1=substr(datarain$TIMESTAMP_END, 7,8)
datarain$hour1=substr(datarain$TIMESTAMP_END,9,10)
datarain$min1=substr(datarain$TIMESTAMP_END,11,12)


#ASSIGN the result of the rename function to the same dataframe
datarain <-  rename(datarain, r=P, 
                    SWC5=SWC_1_1_1,
                    SWC15=SWC_1_2_1,
                    SWC30=SWC_1_3_1,
                    ST5=TS_1_1_1,  
                    ST15=TS_1_2_1, 
                    ST30=TS_1_3_1,
                    AT2=TA_1_2_1,
                    AT6=TA_1_1_1, 
                    RH2=RH_1_2_1,
                    RH6=RH_1_1_1)


# Adding the new columns with dates to the datarain
datarain$dateStart <- paste(datarain$year, datarain$month, datarain$day, sep="-")
datarain$timeStart <- paste(datarain$hour,datarain$min, sep=":")
datarain$dateEnd <- paste(datarain$year1, datarain$month1, datarain$day1, sep="-")
datarain$timeEnd <- paste(datarain$hour1,datarain$min1, sep=":")
datarain$data_time_Start <- paste(datarain$dateStart,datarain$timeStart)
datarain$data_time_End <- paste(datarain$dateEnd,datarain$timeEnd)
datarain$data_time_Start = as.POSIXlt(datarain$data_time_Start, format = "%Y-%m-%d %H:%M")
datarain$data_time_End = as.POSIXlt(datarain$data_time_End, format = "%Y-%m-%d %H:%M")


# High Precipitation column > 5mm rain
datarain$high_precip <- datarain$r>5
datarain$high_precip <- as.numeric(datarain$r>5)
datarain$high_precip <- as.numeric(datarain$r>5)*datarain$r


# making DOY 
datarain$DOY_S <- paste(yday(datarain$dateStart))
datarain$DOY_E <- paste(yday(datarain$dateEnd))

# Choosing the dates with Rain events in DataP
datarain$RainEvent <- paste(datarain$r>0)
datarain$RainEvent <- as.numeric(datarain$r>0)
datarain$Rain_DOY <- as.numeric(datarain$RainEvent)*as.numeric(datarain$DOY_S)
sum(datarain$RainEvent, na.rm=FALSE)


#######################Calculate the rate of rain PER EVENT
plot(datarain$r)

datarain %>%
  filter(RainEvent == 1) %>%
  ggplot(aes(x=DOY_E, y = r))+
  geom_col()

sum(datarain$RainEvent)

datarain %>%
  filter(high_precip > 5) %>%
  ggplot(aes(x=DOY_E, y = r))+
  geom_col()

datarain$highR_event <- datarain$high_precip>5 
datarain$highR_event <- as.numeric(as.logical(datarain$highR_event))
sum(datarain$highR_event) ##### now it's 15 Pulses?! 

datarain$Rain_DOY_high <- as.numeric(datarain$highR_event)*as.numeric(datarain$DOY_S)

New_list_Pulses <- datarain %>%
  filter(highR_event == "TRUE")

######### Loops to Calculate Rain intensity

days = seq(as.Date('2017-01-01'), as.Date('2017-12-31'), by = 'day')

pulse.intensity.fun = function(days) {
  
  # Initialize output data frame
  output = data.frame(matrix(nrow = 0, ncol = 7))
  colnames(output) = c('Date','DOY','RainEvent','n','TotalHours','TotalPrecip','Intensity')
  
  # Loop through days
  for (i in 1 : length(days)){
    
    # Subset by day
    date = days[i]
    datarain.sub = datarain[datarain$dateStart == date, ]
    
    # Get day of year (DOY)
    doy = unique(datarain.sub$DOY_S)
    
    # Get rain event column
    re = datarain.sub$RainEvent
    
    # Get precip column
    r = datarain.sub$r
    
    # Get list of repeating values and corresponding lengths
    re.rle = rle(re)
    
    # Get the total number of events
    nre = sum(re.rle$values)
    
    if (nre != 0) {
      
      print(paste('Calculating pulse intensity for', date))
      # Get start/end index values for each event
      re.end = cumsum(re.rle$lengths)
      re.start = c(1, (re.end[-1] - (re.rle$lengths[-1] - 1)))
      
      # Creat data frame and reduce to only rain events
      re.indices = data.frame(Value = re.rle$values,
                              Length = re.rle$lengths,
                              Index_Start = re.start,
                              Index_End = re.end)
      re.indices = re.indices[re.indices$Value != 0, ]
      
      # Create output data frame for the current day
      output.temp = data.frame(matrix(nrow = nre, ncol = 7))
      colnames(output.temp) = c('Date','DOY','RainEvent','n','TotalHours','TotalPrecip','Intensity')
      output.temp$Date = rep(date, nre)
      output.temp$DOY = rep(doy, nre)
      output.temp$RainEvent = seq(1,nre,1)
      output.temp$n = re.rle$lengths[which(re.rle$values == 1)]
      output.temp$TotalHours = output.temp$n / 2
      
      # Loop through rain events and calculate total precip per each using the predetermined index values
      for (j in 1 : nrow(output.temp)) {
        output.temp$TotalPrecip[j] = sum(r[re.indices$Index_Start[j] : re.indices$Index_End[j]])
      }
      
      # Calculate precip intensity (total precip / total time) for each event
      output.temp$Intensity = output.temp$TotalPrecip / output.temp$TotalHours
      
      # Append output
      output = rbind(output, output.temp)
    }
  }
  print('Done!')
  return(output)
}

pulse.intensity.2017 = pulse.intensity.fun(days)

hist(pulse.intensity.2017$TotalPrecip)

pulse.intensity.2017 %>%
  filter(TotalPrecip > 5) %>%
  ggplot(aes(x = DOY, y = TotalPrecip))+
  geom_col()

pulse.intensity.2017 %>%
  filter(TotalPrecip > 5) %>%
  ggplot(aes(x = DOY, y = Intensity))+
  geom_col()


pulse.intensity.2017 %>%
  filter(TotalPrecip > 5) %>%
  ggplot(aes(x = Intensity, y = TotalPrecip))+
  geom_point(size = 3)+
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(text = element_text(size = 20))


write.csv(file = "pulse.intensity.2017.csv", pulse.intensity.2017)


hist(pulse.intensity.2017$Intensity[pulse.intensity.2017$Intensity>5])




############## Try to get Night-time Reco by using incoming "photosynthetic photon flux density"
####### As I understood it should be == 0

summary3 <- datarain %>%
  group_by(as.numeric(DOY_S)) %>%
  filter(PPFD_IN_F == 0) %>%
  dplyr :: summarise(meanPhoto = mean(PPFD_IN_F, na.rm=TRUE),
                     timeSS = timeStart,
                     meanAT2=mean(replace(AT2, AT2== -9999, NA),na.rm=TRUE), 
                     meanAT6=mean(replace(AT6, AT6== -9999, NA),na.rm=TRUE),
                     sum_R=mean(sum_rain, na.rm=TRUE),
                     rain_events=sum(RainEvent, na.rm=TRUE),
                     meanRH2=mean(replace(RH2, RH2== -9999, NA),na.rm=TRUE),
                     meanRH6=mean(replace(RH6, RH6== -9999, NA),na.rm=TRUE),
                     meanSWC5=mean(replace(SWC5, SWC5== -9999, NA),na.rm=TRUE),
                     meanSWC15=mean(replace(SWC15, SWC15== -9999, NA),na.rm=TRUE),
                     meanSWC30=mean(replace(SWC30, SWC30== -9999, NA),na.rm=TRUE), 
                     meanST5=mean(replace(ST5, ST5== -9999, NA),na.rm=TRUE),
                     meanST15=mean(replace(ST15, ST15== -9999, NA),na.rm=TRUE),
                     meanST30=mean(replace(ST30, ST30== -9999, NA),na.rm=TRUE),
                     meanNEE=mean(NEE, na.rm=TRUE), 
                     meanGPP=mean(GPP, na.rm=TRUE),
                     meanRECO=mean(RECO, na.rm=TRUE), sdReco=sd(RECO, na.rm=TRUE))




# Create a data set for a one pulse 
Pulse5 <- 
  datarain_pro %>%
  filter(DOY_S %in% (169:190)) 

# Calculate the summary for initial conditions one the time after the rain event
# Sum / Average / Daily things
# Sum the rains for each DOY

datarain <- datarain %>%
  group_by(DOY_S) %>%
  mutate(sum_rain = sum(r))


# Save new csv files
write.csv(file="data/Pulse5.csv", Pulse5)

# My version of R does not work with this comand - write_csv >- I made new Pulse5 by using write.csv
write_csv(file="data/datarain_processed.csv", datarain)
write_csv(file="data/Pulse5.csv", Pulse1)


#Plotting SM VS ST for rain events
datarain[datarain == -9999] <- NA
datarain$Season = vector(mode = 'character', length = nrow(datarain))
datarain$Season[datarain$DOY_S %in% c(1:59,305:366)] = 'Winter'
datarain$Season[datarain$DOY_S %in% 60:181] = 'Spring'
datarain$Season[datarain$DOY_S %in% 182:304] = 'Summer'


datarain %>%
  filter(RainEvent==1)%>%
  ggplot(aes(x= SWC5, y= ST5, size = RECO, color = Season)) + 
  geom_point()

datarain %>%
  filter(RainEvent==1)%>%
  ggplot(aes(x= SWC5, y= ST5, size = r, color = Season)) + 
  geom_point()

write.csv(file="data/datarain.csv", datarain)


#Some Mean values for our data
summary2 <- datarain %>%
  group_by(as.numeric(DOY_S)) %>%
  summarise(meanAT2=mean(AT2, na.rm=TRUE), meanAT6=mean(AT6, na.rm=TRUE), sum_R=mean(sum_rain, na.rm=TRUE),
            meanRH2=mean(RH2, na.rm=TRUE),meanRH6=mean(RH6, na.rm=TRUE),meanSWC5=mean(SWC5, na.rm=TRUE),
            meanSWC15=mean(SWC15, na.rm=TRUE), meanSWC30=mean(SWC30, na.rm=TRUE), meanST5=mean(ST5, na.rm=TRUE),
            meanST15=mean(ST15, na.rm=TRUE),meanST30=mean(ST30, na.rm=TRUE),meanNEE=mean(NEE, na.rm=TRUE),
            meanGPP=mean(GPP, na.rm=TRUE),meanRECO=mean(RECO, na.rm=TRUE))
  

# Exclude "-9999" value from the calculations



summary2 <- datarain %>%
  group_by(as.numeric(DOY_S)) %>%
  dplyr :: summarise(meanAT2=mean(replace(AT2, AT2== -9999, NA),na.rm=TRUE), 
            meanAT6=mean(replace(AT6, AT6== -9999, NA),na.rm=TRUE),
            sum_R=mean(sum_rain, na.rm=TRUE),
            rain_events=sum(RainEvent, na.rm=TRUE),
            meanRH2=mean(replace(RH2, RH2== -9999, NA),na.rm=TRUE),
            meanRH6=mean(replace(RH6, RH6== -9999, NA),na.rm=TRUE),
            meanSWC5=mean(replace(SWC5, SWC5== -9999, NA),na.rm=TRUE),
            meanSWC15=mean(replace(SWC15, SWC15== -9999, NA),na.rm=TRUE),
            meanSWC30=mean(replace(SWC30, SWC30== -9999, NA),na.rm=TRUE), 
            meanST5=mean(replace(ST5, ST5== -9999, NA),na.rm=TRUE),
            meanST15=mean(replace(ST15, ST15== -9999, NA),na.rm=TRUE),
            meanST30=mean(replace(ST30, ST30== -9999, NA),na.rm=TRUE),
            meanNEE=mean(NEE, na.rm=TRUE), 
            meanGPP=mean(GPP, na.rm=TRUE),
            meanRECO=mean(RECO, na.rm=TRUE), sdReco=sd(RECO, na.rm=TRUE),
            #meanReco_N= summary3$meanRECO, sdReco_N = summary3$sdReco
            )


summary2$meanReco_N <- summary3$meanRECO
summary2$sdReco_N <- summary3$sdReco

summary2 %>%
  #filter(sum_R >5) %>%
  na.omit()%>%
  ggplot(aes(x=meanRECO, y =meanReco_N))+
  geom_point(size = 3)+
  theme_bw()+
  xlab('Reco')+
  ylab('Reco Night')+
  #coord_cartesian(ylim=c(0, 4), xlim = c(0,5))+
  theme(text = element_text(size = 20))+
  ggtitle('All Reco VS Reco_Night')







summaryrains <- summary2 %>%
  filter(rain_events >0)

#AMOUNTS  of rains in 2017 in Kendall site - 48 days with rain events
nrow(summaryrains)
summaryrains$rain_intens_per_day <- summaryrains$sum_R/summaryrains$rain_events
summaryrains$DeltaRains <- vector(mode="integer", length = length(summaryrains$`as.numeric(DOY_S)`))

for (i in 2: length(summaryrains$DeltaRains)){
  summaryrains$DeltaRains[i]=summaryrains$`as.numeric(DOY_S)`[i]-summaryrains$`as.numeric(DOY_S)`[i-1]
}


plot(summaryrains$DeltaRains, summaryrains$rain_events)

#Time in between the rain events and sum od daily precipitation
plot(summaryrains$DeltaRains, summaryrains$sum_R)

plot(summaryrains$DeltaRains, summaryrains$meanAT2)

plot(summaryrains$DeltaRains, summaryrains$meanRH2)
plot(summaryrains$DeltaRains, summaryrains$meanGPP)

plot(summaryrains$DeltaRains, summaryrains$meanNEE)


# For most of the graphs - more frequent events (less time interval in between the rain events) 
# lead to the higher CO2 fluxes
plot(summaryrains$DeltaRains, summaryrains$meanRECO)


# Add SD + R eco
plot(summaryrains$`as.numeric(DOY_S)`, summaryrains$meanRECO)

ggplot(summaryrains, aes(x=`as.numeric(DOY_S)`, y=meanRECO)) + 
  geom_point()+
  geom_errorbar(aes(ymin=meanRECO - sdReco, ymax=meanRECO + sdReco),
                width=.8, position=position_dodge(0.05))


#I tried to diagnose (statistically) summary2 but this function doesn't work 
summary2 %>%
  diagnose(meanRECO)

# diagnose <- function(.data, ...) {
# UseMethod("diagnose", .data)
# }

diagnose <- function(summary2) {
  UseMethod("diagnose", summary2)
}



sum(summary2$sum_R)

t.test(summary2$meanRECO)

ggplot(mapping=aes(x=summary2$`as.numeric(DOY_S)`,y=summary2$meanRECO))+
  geom_point()

ggplot(mapping=aes(x=summary2$meanSWC5,y=summary2$meanRECO))+
  geom_point()+
  geom_smooth(method = lm, level=0.99)

#Change the names of axis
ggplot(mapping=aes(x=summary2$meanSWC5,y=summary2$meanRECO))+
  geom_point()+
  geom_smooth()+
  xlab(label="SWC 5 cm (%)")+
  ylab(label="Reco (micromol CO2 m-2 s-1)")
  
mean(summary2$meanRECO, na.rm=TRUE)

hist(summary2$sum_R[summary2$sum_R>5]) 

############## Make ggplot histogram !!!

ggplot(summary2, aes(x=sum_R[sum_R>5]))+
  geom_histogram()

summary2 %>%
  filter(sum_R >5) %>%
  ggplot(aes(x=sum_R))+
  geom_histogram(bins = 30)+
  theme_bw()+
  xlab('Rain (mm)')+
  ylab('Frequency (times)')+
  coord_cartesian(ylim=c(0, 4), xlim = c(0,60))+
  theme(text = element_text(size = 20))+
  ggtitle('Rain events > 5 mm')




summary2$Season = vector(mode = 'character', length = nrow(summary2))

summary2$Season[summary2$`as.numeric(DOY_S)` %in% c(1:59,305:366)] = 'Winter'
summary2$Season[summary2$`as.numeric(DOY_S)` %in% 60:181] = 'Spring'
summary2$Season[summary2$`as.numeric(DOY_S)` %in% 182:304] = 'Summer'


# Creating the summary for 18 Pulses
yearPulses18 <- summary2 %>%
  filter(sum_R > 5)


yearPulses18 %>%
  ggplot(aes(x=sum_R))+
  geom_histogram()

yearPulses18 %>%
  ggplot(aes(x= meanSWC5, y= meanST5, size = meanRECO, color = Season)) + 
  geom_point()+ 
  theme_bw()+
  theme(text = element_text(size = 20))+
  xlab(label="Soil Water Content 5 cm (%)")+
  ylab(label="Soil temperature (C)")
  
 

yearPulses18 %>%
  ggplot(aes(x= meanSWC5, y= meanST5, size = sum_R, color = Season)) + 
  geom_point()+
  theme_bw()+
  theme(text = element_text(size = 20))+
  xlab(label="Soil Water Content 5 cm (%)")+
  ylab(label="Soil temperature (C)")

write.csv(file="data/yearPulses18.csv", yearPulses18)

# Count the time interval between the Pulses - in days (using DOY)

yearPulses18$Previous_DOY_rain <- lag(yearPulses18$`as.numeric(DOY_S)`)

yearPulses18$Timeinbetween <- yearPulses18$`as.numeric(DOY_S)`- yearPulses18$Previous_DOY_rain

yearPulses18 %>%
  ggplot(aes(x=Timeinbetween, y=meanRECO))+
  geom_point()+
  theme_classic()

hist(yearPulses18$Timeinbetween)

yearPulses18 %>%
  na.omit() %>%
  ggplot(aes(x=Timeinbetween))+
  geom_histogram()+
  theme_classic()



