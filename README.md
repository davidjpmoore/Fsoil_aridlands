# Fsoil_aridlands
# To start we need to upload data files from eddy-tower to R-studio
# Make some changes in the initial csv-files with data from eddy tower: 
(1) add correct date-format; 
(2) rename some columns - r=P, SWC5=SWC_1_1_1, SWC15=SWC_1_2_1, SWC30=SWC_1_3_1, ST5=TS_1_1_1, ST15=TS_1_2_1, ST30=TS_1_3_1, AT2=TA_1_2_1, AT6=TA_1_1_1, RH2=RH_1_2_1, 
RH6=RH_1_1_1; 
(3) add some columns - DOY (yday command), high_precip (>5 mm)
# In parallel create files wiht just Night-time measurements for further analysis: filter(PPFD_IN == 0) - incoming photosynthetic photon flux density, gap-filled
# Make Summary files with mean data for each DOY: 
summary2017_all <- datarain17 %>%
  group_by(DOY_S) %>%
  na.omit() %>%
  dplyr :: summarise(meanAT2 = mean (replace(AT2, AT2 == -9999, NA),na.rm=TRUE),                     , 
                     meanAT6=mean(replace(AT6, AT6== -9999, NA),na.rm=TRUE),
                     sum_R=sum(r, na.rm=TRUE),
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
                     meanRECO=mean(RECO, na.rm=TRUE), 
                     sdReco=sd(RECO, na.rm=TRUE))
# Mark in each Summary file - Season of year:
summary2017_all$Season = vector(mode = 'character', length = nrow(summary2017_all))
summary2017_all$Season[summary2017_all$DOY_S %in% c(1:59,305:366)] = 'Winter'
summary2017_all$Season[summary2017_all$DOY_S %in% 60:181] = 'Spring'
summary2017_all$Season[summary2017_all$DOY_S %in% 182:304] = 'Summer'
# Add to Summary file few more columns: Rain_DOY, Pulse_Days, Pulse_DOY
# Choose Pulse-time data from each summery file: filter(sum_R > 5) - Sum precipitation per day > 5 mm
# Create Non-pulse time data sets: filter(Pulse_DOY == 0) - Data set without Pulse events
# Check Correlation between Fluxes (NEE, GPP, Reco) and environmental variables

####### Soil Respiration Part #########
# Take initial excel-files from chamber observations
# Make chamges in date format
# Create Summary file for each DOY - mean values:
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
# Add column "Season" to Summury file:
soildata_new$Season = vector(mode = 'character', length = nrow(soildata_new))
soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% c(1:59,305:366)] = 'Winter'
soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% 60:181] = 'Spring'
soildata_new$Season[soildata_new$`as.numeric(DOY)` %in% 182:304] = 'Summer'


####### MODELING PART ###########
# Combine all 4 summary csv-files
# Create two files for Pulse - and Non-Pulse time: 
years_sum_Pulse0 <- years_sum1 %>%
  filter(Pulse_DOY == 0)

years_sum_Pulse1 <- years_sum1 %>%
  filter(Pulse_DOY > 0)
  
# Model Application....

 







