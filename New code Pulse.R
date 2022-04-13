#4/12/2022
#Anastasia Makhnykina

#packages we need
library(dplyr)
library(lubridate)
library(skimr)
library(data.table)


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

datarain <-  rename(datarain,SWC15=SWC_1_2_1)

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
  datarain %>%
  filter(DOY_S %in% (175:186)) 

# Calculate the summary for initial conditions one the time after the rain event
# Sum / Average / Daily things

#Sum the rains for each DOY

datarain <- datarain %>%
  group_by(DOY_S) %>%
  mutate(sum_rain = sum(r))



                    
                    
                    
                    
                    

