# Rain Intensity coding
# 09-06-2022


rain = read.csv('C:/Users/charlie/Desktop/datarain.csv')

days = seq(as.Date('2017-01-01'), as.Date('2017-12-31'), by = 'day')

pulse.intensity.fun = function(days) {
  
  # Initialize output data frame
  output = data.frame(matrix(nrow = 0, ncol = 8)) # ~~~~ Update number of columns when new variable is added
  colnames(output) = c('Date','DOY','RainEvent','n','TotalHours','TotalPrecip','Intensity','Mean_SWC5cm') # ~~~~ Add new column names for new variables
  
  # Loop through days
  for (i in 1 : length(days)){
    
    # Subset by day
    date = days[i]
    rain.sub = rain[rain$dateStart == date, ]
    
    # Get day of year (DOY)
    doy = unique(rain.sub$DOY_S)
    
    # Get rain event column
    re = rain.sub$RainEvent
    
    # Get precip column
    r = rain.sub$r
    
    # Get list of repeating values and corresponding lengths
    re.rle = rle(re)
    
    # Get the total number of events
    nre = sum(re.rle$values)
    
    # Get other variables
    swc5 = rain.sub$SWC5     # ~~~~ Keep adding new variables just like this one
    
    
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
      output.temp = data.frame(matrix(nrow = nre, ncol = 8)) # ~~~~ Update number of columns when new variable is added
      colnames(output.temp) = c('Date','DOY','RainEvent','n','TotalHours','TotalPrecip','Intensity','Mean_SWC5cm') # ~~~~ Add new column names for new variables
      output.temp$Date = rep(date, nre)
      output.temp$DOY = rep(doy, nre)
      output.temp$RainEvent = seq(1,nre,1)
      output.temp$n = re.rle$lengths[which(re.rle$values == 1)]
      output.temp$TotalHours = output.temp$n / 2
      
      # Loop through rain events and calculate total precip per each using the predetermined index values
      for (j in 1 : nrow(output.temp)) {
        output.temp$TotalPrecip[j] = sum(r[re.indices$Index_Start[j] : re.indices$Index_End[j]])
        # ~~~~ New column names here
        output.temp$Mean_SWC5cm[j] = mean(swc5[re.indices$Index_Start[j] : re.indices$Index_End[j]])
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



