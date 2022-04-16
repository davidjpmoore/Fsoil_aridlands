#4/12/2022
#Anastasia Makhnykina

#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)

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
datarain=read.csv("data/20162017Meteo.csv", header=TRUE, na.strings = "NaN")


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


#high Precipitation column
datarain$high_precip <- datarain$r>5
datarain$high_precip <- as.numeric(datarain$r>5)
datarain$high_precip <- as.numeric(datarain$r>5)*datarain$r


#making DOY 
datarain$DOY_S <- paste(yday(datarain$dateStart))
datarain$DOY_E <- paste(yday(datarain$dateEnd))

# Choosing the dates with Rain events in DataP

datarain$RainEvent <- paste(datarain$r>0)
datarain$RainEvent <- as.numeric(datarain$r>0)

datarain$Rain_DOY <- as.numeric(datarain$RainEvent)*as.numeric(datarain$DOY_S)

sum(datarain$RainEvent, na.rm=FALSE)

#Create a data set for a one pulse 
Pulse1 <- 
  datarain_pro %>%
  filter(DOY_S %in% (169:191)) 

# Calculate the summary for initial conditions one the time after the rain event
# Sum / Average / Daily things

#Sum the rains for each DOY

datarain <- datarain %>%
  group_by(DOY_S) %>%
  mutate(sum_rain = sum(r))

#Save new csv files
write.csv(file="data/Pulse1.csv", Pulse1)

# My version of R does not work with this comand - write_csv >- I made new Pulse1 by using write.csv
write_csv(file="data/datarain_processed.csv", datarain)
write_csv(file="data/Pulse1.csv", Pulse1)

#Some Mean values for our data
summary1 <- datarain %>%
  group_by(as.numeric(DOY_S)) %>%
  summarise(sum_rain = sum(r))


summary2 <- datarain %>%
  group_by(DOY_S) %>%
  select(DOY_S, DOY_E, data_time_Start, data_time_End, r, RainEvent, Rain_DOY, sum_rain, AT2, AT6, RH2, RH6,
         SWC5, SWC15, SWC30, ST5, ST15, ST30, NEE, GPP, RECO)

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
  summarise(meanAT2=mean(replace(AT2, AT2== -9999, NA),na.rm=TRUE), 
            meanAT6=mean(replace(AT6, AT6== -9999, NA),na.rm=TRUE),
            sum_R=mean(sum_rain, na.rm=TRUE),
            meanRH2=mean(replace(RH2, RH2== -9999, NA),na.rm=TRUE),
            meanRH6=mean(replace(RH6, RH6== -9999, NA),na.rm=TRUE),
            meanSWC5=mean(replace(SWC5, SWC5== -9999, NA),na.rm=TRUE),
            meanSWC15=mean(replace(SWC15, SWC15== -9999, NA),na.rm=TRUE),
            meanSWC30=mean(replace(SWC30, SWC30== -9999, NA),na.rm=TRUE), 
            meanST5=mean(replace(ST5, ST5== -9999, NA),na.rm=TRUE),
            meanST15=mean(replace(ST15, ST15== -9999, NA),na.rm=TRUE),
            meanST30=mean(replace(ST30, ST30== -9999, NA),na.rm=TRUE),
            meanNEE=mean(NEE, na.rm=TRUE), meanGPP=mean(GPP, na.rm=TRUE),meanRECO=mean(RECO, na.rm=TRUE))


summary2 %>%
  diagnose(meanRECO)

sum(summary2$sum_R)
sum(datarain_pro$r)

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
  
                    
                    

