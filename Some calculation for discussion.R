# Anastasia Makhnykina
# 09-10-2025
# Calculating the seasons input


# We need documents with 0/1 - Pulse time, years_sum1

years_sum_Pulse0$DOY <- paste(yday(years_sum_Pulse0$date))
years_sum_Pulse0$DOY <- as.numeric(as.character(years_sum_Pulse0$DOY))

years_sum_Pulse1$DOY <- paste(yday(years_sum_Pulse1$date))
years_sum_Pulse1$DOY <- as.numeric(as.character(years_sum_Pulse1$DOY))

years_sum1$DOY <- paste(yday(years_sum1$date))
years_sum1$DOY <- as.numeric(as.character(years_sum1$DOY))

# TTTTttttt ##################

years_sum_Pulse0$Season = vector(mode = 'character', length = nrow(years_sum_Pulse0))
years_sum_Pulse0$Season[years_sum_Pulse0$DOY %in% c(1:59,305:366)] = 'Winter'
years_sum_Pulse0$Season[years_sum_Pulse0$DOY %in% 60:181] = 'Spring'
years_sum_Pulse0$Season[years_sum_Pulse0$DOY %in% 182:304] = 'Summer'

years_sum_Pulse1$Season = vector(mode = 'character', length = nrow(years_sum_Pulse1))
years_sum_Pulse1$Season[years_sum_Pulse1$DOY %in% c(1:59,305:366)] = 'Winter'
years_sum_Pulse1$Season[years_sum_Pulse1$DOY %in% 60:181] = 'Spring'
years_sum_Pulse1$Season[years_sum_Pulse1$DOY %in% 182:304] = 'Summer'

years_sum1$Season = vector(mode = 'character', length = nrow(years_sum1))
years_sum1$Season[years_sum1$DOY %in% c(1:59,305:366)] = 'Winter'
years_sum1$Season[years_sum1$DOY %in% 60:181] = 'Spring'
years_sum1$Season[years_sum1$DOY %in% 182:304] = 'Summer'


meanWinterReco0 <- years_sum1 %>% 
  filter(Season == 'Winter') %>%
  filter(days_since_rain_event >= max_pulse_duration) 

meanWinterReco0$meanRECO <- as.numeric(as.character(meanWinterReco0$meanRECO))

sum(meanWinterReco0$meanRECO, na.rm=TRUE)
sd(meanWinterReco0$meanRECO, na.rm=TRUE)
  

meanWinterReco1 <- years_sum1 %>% 
  filter(Season == 'Winter') %>%
  filter(days_since_rain_event < max_pulse_duration) 

meanWinterReco1$meanRECO <- as.numeric(as.character(meanWinterReco1$meanRECO))

sum(meanWinterReco1$meanRECO, na.rm=TRUE)
sd(meanWinterReco1$meanRECO, na.rm=TRUE)












meanSpringReco0 <- years_sum1 %>% 
  filter(Season == 'Spring') %>%
  filter(days_since_rain_event >= max_pulse_duration) 

meanSpringReco0$meanRECO <- as.numeric(as.character(meanSpringReco0$meanRECO))

sum(meanSpringReco0$meanRECO, na.rm=TRUE)
sd(meanSpringReco0$meanRECO, na.rm=TRUE)


meanSpringReco1 <- years_sum1 %>% 
  filter(Season == 'Spring') %>%
  filter(days_since_rain_event < max_pulse_duration) 

meanSpringReco1$meanRECO <- as.numeric(as.character(meanSpringReco1$meanRECO))

sum(meanSpringReco1$meanRECO, na.rm=TRUE)
sd(meanSpringReco1$meanRECO, na.rm=TRUE)





meanSummerReco0 <- years_sum1 %>% 
  filter(Season == 'Summer') %>%
  filter(days_since_rain_event >= max_pulse_duration) 

meanSummerReco0$meanRECO <- as.numeric(as.character(meanSummerReco0$meanRECO))

sum(meanSummerReco0$meanRECO, na.rm=TRUE)
sd(meanSummerReco0$meanRECO, na.rm=TRUE)


meanSummerReco1 <- years_sum1 %>% 
  filter(Season == 'Summer') %>%
  filter(days_since_rain_event < max_pulse_duration) 

meanSummerReco1$meanRECO <- as.numeric(as.character(meanSummerReco1$meanRECO))

sum(meanSummerReco1$meanRECO, na.rm=TRUE)
sd(meanSummerReco1$meanRECO, na.rm=TRUE)









