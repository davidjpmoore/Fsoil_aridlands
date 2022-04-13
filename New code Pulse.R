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

#Rename these columns
rename(datarain, r=P)
rename(datarain, SWC5=SWC_1_1_1)
rename(datarain, SWC15=SWC_1_2_1)
rename(datarain, SWC30=SWC_1_3_1)
rename(datarain, ST5=TS_1_1_1)
rename(datarain, ST15=TS_1_2_1)
rename(datarain, ST30=TS_1_3_1)
rename(datarain, AT2=TA_1_2_1)
rename(datarain, AT6=TA_1_1_1)
rename(datarain, RH2=RH_1_2_1)
rename(datarain, RH6=RH_1_1_1)

#For some reasons THESE FUNCTIONS DO NOT WORK

#I did not delete this data frame because I still do not have these in datarain Data set
dataP <- data.frame(year=Years,
                    month=Months,
                    day=Days,
                    hour=Time_hour,
                    min=Time_min,
                    year1=Years_end,
                    month1=Months_end,
                    day1=Days_end,
                    hour1=Time_hend,
                    min1=Time_mend,
                    r=datarain$P,
                    SWC5=datarain$SWC_1_1_1,
                    SWC15=datarain$SWC_1_2_1,
                    SWC30=datarain$SWC_1_3_1,
                    ST5=datarain$TS_1_1_1,
                    ST15=datarain$TS_1_2_1,
                    ST30=datarain$TS_1_3_1,
                    AT2=datarain$TA_1_2_1,
                    AT6=datarain$TA_1_1_1,
                    CO2=datarain$CO2,
                    H2O=datarain$H2O,
                    RH2=datarain$RH_1_2_1,
                    RH6=datarain$RH_1_1_1,
                    NEE=datarain$NEE,
                    Reco=datarain$RECO,
                    GPP=datarain$GPP)
dataP$dateStart <- paste(dataP$year, dataP$month, dataP$day, sep="-")
dataP$timeStart <- paste(dataP$hour,dataP$min, sep=":")
dataP$dateEnd <- paste(dataP$year1, dataP$month1, dataP$day1, sep="-")
dataP$timeEnd <- paste(dataP$hour1,dataP$min1, sep=":")


dataP$data_time_Start <- paste(dataP$dateStart,dataP$timeStart)
dataP$data_time_End <- paste(dataP$dateEnd,dataP$timeEnd)

dataP$data_time_Start = as.POSIXlt(dataP$data_time_Start, format = "%Y-%m-%d %H:%M")
dataP$data_time_End = as.POSIXlt(dataP$data_time_End, format = "%Y-%m-%d %H:%M")


#high Precipitation column
dataP$high_precip <- dataP$r>5
dataP$high_precip <- as.numeric(dataP$r>5)
dataP$high_precip <- as.numeric(dataP$r>5)*dataP$r



#making DOY 
dataP$DOY_S <- paste(yday(dataP$dateStart))
dataP$DOY_E <- paste(yday(dataP$dateEnd))

# Choosing the dates with Rain events in DataP

dataP$RainEvent <- paste(dataP$r>0)
dataP$RainEvent <- as.numeric(dataP$r>0)

dataP$Rain_DOY <- as.numeric(dataP$RainEvent)*as.numeric(dataP$DOY_S)

sum(dataP$RainEvent, na.rm=FALSE)

#Create a data set for a one pulse 
Pulse1 <- 
  dataP %>%
  filter(DOY_S %in% (175:186)) 

# Calculate the summary for initial conditions one the time after the rain event
# Sum / Average / Daily things

#library(dplyr)
dataP %>%
  group_by(DOY_S) %>%
  dplyr::mutate(sum_rain = sum(r))




