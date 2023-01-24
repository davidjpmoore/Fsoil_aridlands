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
library(viridis)
library(RColorBrewer)
library(broom)
library(ggpubr)
library(ggpmisc)



#open cvs
data_Chambers=read.csv("C:/Users/sunlife1408/Documents/RainMan2022/Fsoil_aridlands/data/kendall2017.csv", 
                       header=TRUE, na.strings = "NaN", sep = ";")

# Calculate DOY
data_Chambers$date <- paste(data_Chambers$year, data_Chambers$month, data_Chambers$day, sep="-")
data_Chambers$time <- paste(data_Chambers$hour,data_Chambers$min, sep=":")

data_Chambers$date <- ymd(as.character(data_Chambers$date))
data_Chambers$DOY <- paste(yday(data_Chambers$date))

data_Chambers$date <- as_date(data_Chambers$date)
data_Chambers$DOY <- as.numeric(as.character(data_Chambers$DOY))




#Calculate the mean Rsoil for each DOY
dataRsoil <- data_Chambers %>%
  group_by(DOY) %>%
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



#plot(summary2$meanRECO, dataRsoil$meanSR1)
plot(dataRsoil$DOY)

#Create the empty rows from DOY = 331 to DOU = 354 - I need 22 empty rows == NA

dataRsoil$Date = as.Date(dataRsoil$'as.numeric(DOY)' -1, '2017-01-01')

soildata_new <- dataRsoil %>%
  complete(Date = seq(as.Date('2017-01-01'),as.Date('2017-12-31'),by='day'))

soildata_new$'as.numeric(DOY)' = seq(1, nrow(soildata_new), by = 1)


soildata_new <- soildata_new %>%
  mutate(MeanReco = summary2$meanRECO)

soildata_new <- soildata_new %>%
  mutate(MeanGPP = summary2$meanGPP)

soildata_new$meanReco_N <- summary3$meanRECO


soildata_new$Season = vector(mode = 'character', length = nrow(soildata_new))

soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% c(1:59,305:366)] = 'Winter'
soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% 60:181] = 'Spring'
soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% 182:304] = 'Summer'


soildata_new%>%
  na.omit() %>%
  filter(Pulse_day == 'TRUE') %>%
  ggplot(aes(x=MeanGPP, y=meanSR1, color = Season, size = meanSM1)) + 
  geom_point()+
  theme_classic()+
  ylab('Rsoil (micromol CO2 m-2 s-1)')+
  xlab('GPP')+
  theme(text = element_text(size = 20))


soildata_new$Pulse_day <- vector(mode = 'character', length = nrow(soildata_new))

soildata_new$Pulse_day[soildata_new$`as.numeric(DOY)` %in% c(14,15,21,172,176,184,190,
                                               193,194,198,207,208,209,213,214,222,
                                               224,351)] = 'TRUE'



soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=MeanGPP, y=meanSR1, color = Pulse_day, size = meanSM1)) + 
  geom_point()+
  theme_classic()+
  ylab('Soil emission [micromol CO2 m-2 s-1]')+
  xlab('GPP')+
  stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2), 
              size = 1)+
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        #formula = log(y) ~ x
                        )
soildata_new$mean_SM <- vector(mode = 'numeric', length = nrow(soildata_new))
soildata_new$mean_SM <- rowMeans(soildata_new [, c(11:14), na.rm=TRUE])



soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=MeanGPP, y=MeanReco, #color = Pulse_day, 
             size = mean_SM)) + 
  geom_point(color = "red4")+
  ylab('Reco [micromol CO2 m-2 s-1]')+
  xlab('GPP')+
  stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2), 
              size = 1)+
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  theme_bw()+
  theme(text = element_text(size = 20))

soildata_new$meanSR <- rowMeans(soildata_new [, c(3,4,5,6)])


soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=MeanGPP, y=meanSR, color = Season, size = mean_SM)) + 
  geom_point()+
  theme_classic()+
  ylab('Soil emission [micromol CO2 m-2 s-1]')+
  xlab('GPP')

soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=MeanGPP, y=meanSR, color = Pulse_day, size = meanSR)) + 
  geom_point()+
  theme_classic()+
  ylab('Soil emission [micromol CO2 m-2 s-1]')+
  xlab('GPP')

############ Add Days with Pulses to the graph above


#soildata_new$mean_SR <- vector(mode = 'numeric', length = nrow(soildata_new))
#soildata_new$mean_SR <- rowMeans(soildata_new [, c(3:6), na.rm=TRUE])


soildata_new%>%
  na.omit() %>%
  filter(Pulse_day == 'TRUE') %>%
  ggplot(aes(x=MeanGPP, y=meanSR1, color = Season, size = meanSM1)) + 
  geom_point()+
  theme_classic()+
  ylab('Soil emission [micromol CO2 m-2 s-1]')+
  xlab('GPP')
  

soildata_new%>%
  na.omit() %>%
  filter(Pulse_day == 'TRUE') %>%
  ggplot(aes(x=MeanGPP, y=meanSR, color = Season, size = mean_SM)) + 
  geom_point()+
  theme_classic()+
  ylab('Soil emission [micromol CO2 m-2 s-1]')+
  xlab('GPP')


soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=MeanGPP, y=meanSR1, size = meanSM1)) + 
  geom_point()+
  geom_smooth()+
  theme_classic()


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



soildata_new%>%
  na.omit() %>%
  group_by(Season) %>%
  ggplot(aes(x=meanSR, y=MeanReco, color = Season)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')+
  theme(text = element_text(size = 20))


soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=meanSR, y=MeanReco)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')+
  theme(text = element_text(size = 20))


soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=meanSR, y=meanReco_N, color = Season)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')+
  theme(text = element_text(size = 20))+
  xlab('Mean Soil Respiration')+
  ylab('Reco Night')+
  #coord_cartesian(ylim=c(0, 4), xlim = c(0,5))+
  theme(text = element_text(size = 20))+
  ggtitle('SR VS Reco_Night')+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))
  
  
soildata_new%>%
  na.omit() %>%
  ggplot(aes(x=meanSR, y=meanReco_N)) + 
  geom_point()+
  stat_smooth(method = 'lm')+
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')+
  theme(text = element_text(size = 20))+
  xlab('Mean Soil Respiration')+
  ylab('Reco Night')+
  #coord_cartesian(ylim=c(0, 4), xlim = c(0,5))+
  theme(text = element_text(size = 20))+
  ggtitle('SR VS Reco_Night')+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))




# Three figures in one

soildata_new%>%
  na.omit() %>%
  filter(Season == 'Summer') %>%
# Why this next pert is doesn'r work? 
  ggplot(aes(x = Date , y = meanSM1)) + 
  geom_point(shape=1)+
  theme_classic()
  
  
  facet_grid(~)


plot (soildata_new$`as.numeric(DOY)`, soildata_new$meanSR1
    )








