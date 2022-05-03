#4/27/2022
#Anastasia Makhnykina

#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)



#open cvs
data_Chambers=read.csv("data/kendall2017.csv", header=TRUE, na.strings = "NaN", sep = ";")

# Calculate DOY
data_Chambers$date <- paste(data_Chambers$year, data_Chambers$month, data_Chambers$day, sep="-")
data_Chambers$time <- paste(data_Chambers$hour,data_Chambers$min, sep=":")
data_Chambers$DOY <- paste(yday(data_Chambers$date))

data_Chambers$date <- as_date(data_Chambers$date)


#Calculate the mean Rsoil for each DOY
dataRsoil <- data_Chambers %>%
  group_by(as.numeric(DOY)) %>%
  dplyr::summarise(meanSR1=mean(Fs_1, na.rm=TRUE), 
            meanSR2=mean(Fs_2, na.rm=TRUE), 
            meanSR3=mean(Fs_3, na.rm=TRUE),
            meanSR4=mean(Fs_4, na.rm=TRUE),
            meanST1=mean(Ts_1, na.rm=TRUE), 
            meanST2=mean(TFs_2, na.rm=TRUE), 
            meanST3=mean(TFs_3, na.rm=TRUE), 
            meanST4=mean(Ts_4, na.rm=TRUE), 
            meanSM1=mean(VWC_1, na.rm=TRUE), 
            meanSM2=mean(VWC_2, na.rm=TRUE),
            meanSM3=mean(VWC_3, na.rm=TRUE), 
            meanSM4=mean(VWC_4, na.rm=TRUE))


write.csv(file="data/dataRsoil.csv", dataRsoil)



plot(summary2$meanRECO, dataRsoil$meanSR1)
plot(dataRsoil$`as.numeric(DOY)`)

#Create the empty rows from DOY = 331 to DOU = 354 - I need 22 empty rows == NA
library()

dataRsoil$Date = as.Date(dataRsoil$'as.numeric(DOY)'-1, '2017-01-01')

soildata_new <- dataRsoil %>%
  complete(Date = seq(as.Date('2017-01-01'),as.Date('2017-12-31'),by='day'))

soildata_new$'as.numeric(DOY)' = seq(1, nrow(soildata_new), by = 1)


soildata_new <- soildata_new %>%
  mutate(MeanReco = summary2$meanRECO)

soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=meanSR1, y=MeanReco, color = Season)) + 
  geom_point()



soildata_new$Season = vector(mode = 'character', length = nrow(soildata_new))

soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% c(1:59,305:366)] = 'Winter'
soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% 60:181] = 'Spring'
soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% 182:304] = 'Summer'


soildata_new%>%
  na.omit() %>%
  group_by(Season) %>%
  ggplot(aes(x=meanSR1, y=MeanReco, color = Season)) + 
  geom_point()+
  geom_smooth()


soildata_new%>%
  na.omit() %>%
  group_by(Season) %>%
  ggplot(aes(x=meanSR1, y=MeanReco, color = Season)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')


soildata_new%>%
  na.omit() %>%
  group_by(Season) %>%
  ggplot(aes(x=meanSR2, y=MeanReco, color = Season)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')

soildata_new%>%
  na.omit() %>%
  group_by(Season) %>%
  ggplot(aes(x=meanSR3, y=MeanReco, color = Season)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')


soildata_new%>%
  na.omit() %>%
  group_by(Season) %>%
  ggplot(aes(x=meanSR4, y=MeanReco, color = Season)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')

# Calculate the mean SR 

soildata_new$meanSR <- rowMeans(soildata_new [, c(3,4,5,6)])

soildata_new%>%
  na.omit() %>%
  group_by(Season) %>%
  ggplot(aes(x=meanSR, y=MeanReco, color = Season)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')


soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=meanSR, y=MeanReco)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')










