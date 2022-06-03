#load Kendall data in R
library(dplyr)
library(lubridate)
library(skimr)
library(data.table)


#open cvs
datarain=read.csv("data/20162017Meteo.csv", header=TRUE, na.strings = "NaN")

# read precip. column from dataset
r = datarain$P
r[r == 0] = NA


#mean value 
rmean = mean(r ,na.rm = TRUE)

#sd value
rsd = sd(r, na.rm = TRUE)


#data type
typeof(datarain$P)

#change data type
datarain$P<- as.numeric(as.double(datarain$P))

#normal ditribution
dist=dnorm(r,mean=rmean,sd=rsd)
plot(dist, type = 'l')

x = r
y = dist

plot(x, y,
     xlab = 'Half-Hourly Precipitation [mm]',
     ylab = 'Normal Distribution')

hist(r, breaks = 50,
     xlab = 'Half-Hourly Precipitation [mm]')

#simple filtering

datarain_positive <- datarain  %>% # Choosing the Batting Table 
  # AND
  filter(P > 0) 


#substr
#to parse bits of strings
# year_1=substr(datarain$TIMESTAMP_START,1,4)
YEARTEST1=substr(datarain$TIMESTAMP_START,1,4)
HIGHPRECIP= filter(datarain, r>6)
plot(HIGHPRECIP$TIMESTAMP_START, HIGHPRECIP$P)


#paste or past0
#to add different columns together
#paste("one", "two", sep="")
#paste("one", "two", sep=",")


#Date of Start and End of Measurements 
Years_end=substr(datarain$TIMESTAMP_END,1,4)
Months_end=substr(datarain$TIMESTAMP_END, 5,6)
Days_end=substr(datarain$TIMESTAMP_END, 7,8)
Time_hend=substr(datarain$TIMESTAMP_END,9,10)
Time_mend=substr(datarain$TIMESTAMP_END,11,12)

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


dataP_June <- data.frame (date1=dataP$data_time_Start,
                          date2=dataP$data_time_End,
                          r=datarain$P,
                          rH=dataP$high_precip)

dataP_June$date1 = as.POSIXlt(dataP_June$date1, format = "%Y-%m-%d %H:%M")
dataP_June$date2 = as.POSIXlt(dataP_June$date2, format = "%Y-%m-%d %H:%M")

startDate = as.POSIXlt('2017-06-25 16:00', format = "%Y-%m-%d %H:%M")
endDate = as.POSIXlt('2017-07-05 16:00', format = "%Y-%m-%d %H:%M")

#dataP_June %>% filter(dataP_June$data1 >= "2017-06-25 16:00"& dataP_June$data1 <= "2017-07-05 16:00")
#dataP_June %>% filter(dataP_June, date1 >= startDate, date1 <= endDate)

#subsetting the data frame by dates
dataP_June = dataP_June[dataP_June$date1 >= startDate,]
dataP_June = dataP_June[dataP_June$date1 <= endDate,]

dataP_6_Pulse1 <- data.frame (dateS=dataP$data_time_Start,
                             dateE=dataP$data_time_End, 
                             r=datarain$P, 
                             #rH=dataP_June$rh,
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

dataP_6_Pulse1 = dataP_6_Pulse1[dataP_6_Pulse1$dateS >= startDate,]
dataP_6_Pulse1 = dataP_6_Pulse1[dataP_6_Pulse1$dateE <= endDate,]

dataP_6_Pulse1$PH = dataP_June$rH[1:480]

x1=dataP_6_Pulse1$dateS
y1=dataP_6_Pulse1$SWC5
plot(x1,y1,
     xlab='Time',
     ylab='SWC 5 cm')

x2=dataP_6_Pulse1$dateS
y2=dataP_6_Pulse1$ST5
plot(x2,y2,
     xlab='Time',
     ylab='Soil temperature 5 cm')

x3=dataP_6_Pulse1$dateS
y3=dataP_6_Pulse1$AT2
plot(x3,y3,
     xlab='Time',
     ylab='Air temperature 2 m')

x4=dataP_6_Pulse1$dateS
y4=dataP_6_Pulse1$CO2
plot(x4,y4,
     xlab='Time',
     ylab='CO2 concentration [ppm]')

x5=dataP_6_Pulse1$dateS
y5=dataP_6_Pulse1$H2O
plot(x5,y5,
     xlab='Time',
     ylab='H2O')

x6=dataP_6_Pulse1$dateS
y6=dataP_6_Pulse1$RH2
plot(x6,y6,
     xlab='Time',
     ylab='Relative Humidity 2m')

x7=dataP_6_Pulse1$dateS
y7=dataP_6_Pulse1$NEE
plot(x7,y7,
     xlab='Time',
     ylab='NEE')

x8=dataP_6_Pulse1$dateS
y8=dataP_6_Pulse1$Reco
plot(x8,y8,
     xlab='Time',
     ylab='Reco [micromol CO2 m-2 s-1]')

x9=dataP_6_Pulse1$dateS
y9=dataP_6_Pulse1$GPP
plot(x9,y9,
     xlab='Time',
     ylab='GPP [micromol CO2 m-2 s-1]')

x10=dataP_6_Pulse1$ST5 
y10=dataP_6_Pulse1$Reco
plot(x10,y10,
     xlab='Soil temperature 5cm',
     ylab='Reco [micromol CO2 m-2 s-1]')

x11=dataP_6_Pulse1$dateS 
y11=dataP_6_Pulse1$r
plot(x11,y11,
     xlab='Time',
     ylab='Rain [mm]')

cor(dataP_6_Pulse1$SWC5, dataP_6_Pulse1$Reco, method = c("pearson"))
cor(dataP_6_Pulse1$ST30, dataP_6_Pulse1$Reco, method = c("pearson"))
cor(dataP_6_Pulse1$AT2, dataP_6_Pulse1$Reco, method = c("pearson"))
cor(dataP_6_Pulse1$NEE, dataP_6_Pulse1$Reco, method = c("pearson"))
cor(dataP_6_Pulse1$GPP, dataP_6_Pulse1$Reco, method = c("pearson"))

a=dataP$data_time_Start
b=dataP$r
plot(a,b,
     alab='Time',
     blab='Rain [mm]')

#summery about your dataset

library(skimr)
install.packages("skimr")
skim(dataP)

#making DOY 
dataP$DOY_S <- paste(yday(dataP$dateStart))
dataP$DOY_E <- paste(yday(dataP$dateEnd))

# 1. Create classification of the rain events based on the rain event in mm: 
# o-0.5 mm, >0.5 - 1 mm, >1-5 mm, >5-10 mm, >10-20 mm, >20-30 mm, >30-40 mm  

# 2. Making the data set for each event including: 
# Date S/E, DOY_S/E, NEE, Reco, GPP, Ts 5, 15, 30, cm, SM 5,15,30 cm, AT 2m, RH 2m

# 3. Create the lag function to look the initial conditions of each rain event

# 4. DOY of the seasons:
# DOY = 60-181 - Spring 
# DOY = 305-59 - Winter
# DOY = 182-304 - Summer 
# Make classification based on the season, initial conditions and size of the rain event



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









