
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)

#Read TowerFlux data with information about pulse length in it

pulsedefinitions<- read.csv("data/years_sum1_DM.csv", 
                            header=TRUE, na.strings = "NaN") %>%
#retain only variables that I'm interested in.
    select(date,days_since_rain_event,max_pulse_duration, sum_R, meanSWC5, meanST5, meanGPP, meanRECO) %>%
  mutate(pulseIND = days_since_rain_event < max_pulse_duration)  %>%
  mutate(meanSWC5 = as.numeric(meanSWC5)/100) %>%
  mutate(meanST5 = as.numeric(meanST5))  %>%
  mutate(meanGPP = as.numeric(meanGPP))%>%
  #converting RECO into µmol_CO2_m2_s
  mutate(meanRECO = as.numeric(meanRECO)*(1 / 12) * (1 / 1e-6) * (1 / 86400)) 

#read soil chamber data provided by Russ Scott
CH_USWkg17_20 <- read.csv("data/KN_soil_resp17_20_longHead.csv", header=TRUE, na.strings="NaN", skip=0)
#check date format
CH_USWkg17_20$date = as.Date(paste(CH_USWkg17_20$Year+2000, CH_USWkg17_20$DOY, sep = "-"), format = "%Y-%j")

  # Merge data frames by date
Cham_USWKG_pulsedata <- CH_USWkg17_20 %>%
  left_join(mutate(pulsedefinitions, date = as.Date(date)), by = "date")

  
 RchamberVariables= colnames(Cham_USWKG_pulsedata)
 #column names of the orinal file contains non-standard labeling
 # [1] "date"                        "Year"                        "DOY"                        
 # [4] "Port.1Soil.Resp.um.co2.m2.s" "Port.1VWC"                   "Port.1Soil.Temp.deg.C"      
 # [7] "Port.2soil.respum.co2.m2.s"  "Port.2VWC"                   "Port.2Soil.Temp.deg.C"      
 # [10] "Port.3soil.respum.co2.m2.s"  "Port.3VWC"                   "Port.3Soil.Temp.deg.C"      
 # [13] "Port.4Soil.Resp.um.co2.m2.s" "Por4VWC"                     "Port.4Soil.Temp.deg.C"      
 # [16] "Port.5soil.respum.co2.m2.s"  "Port.5VWC"                   "Port.5Soil.Temp.deg.C"      
 # [19] "Port.6soil.respum.co2.m2.s"  "Port.6VWC"                   "Port.6Soil.Temp.deg.C"      
 # [22] "Port.7soil.respum.co2.m2.s"  "Port.7VWC"                   "Port.7Soil.Temp.deg.C"      
 # [25] "days_since_rain_event"       "max_pulse_duration"          "sum_R"                      
 # [28] "meanSWC5"                    "meanST5"                     "meanGPP"                    
 # [31] "pulseIND"
 
 #Create new column names
 #
 #.    WARNING THIS CHANGES THE COLUMN NAMES BASED ON THE ORDER THEY ARE READ
 #          CHECK THEM!
 
 newnames = c("date",	"Year",	"DOY",	"Rsoil1",	"VWC1",	"Tsoil1",	"Rsoil2",	"VWC2",	"Tsoil2",	
 "Rsoil3",	"VWC3",	"Tsoil3",	"Rsoil4",	"VWC4",	"Tsoil4",	 "Rsoil5",	"VWC5",	"Tsoil5",	
 "Rsoil6",	"VWC6",	"Tsoil6",	"Rsoil7",	"VWC7",	"Tsoil7",	"days_since_rain_event",
 "max_pulse_duration",	"sum_R",	"meanSWC5",	"meanST5",	"meanGPP", "meanRECO",	"pulseIND")
 
 
 column_units = list("days",	"years",	"days",	"µmol_CO2_m2_s",	"VWC",	"degC",	"µmol_CO2_m2_s",	"VWC",	"degC",
                     "µmol_CO2_m2_s",	 "VWC",	"degC",	"µmol_CO2_m2_s",	"VWC",	"degC",	"µmol_CO2_m2_s",	
                     "VWC",	"degC",	"µmol_CO2_m2_s",	"VWC",	"degC",	 "µmol_CO2_m2_s",	"VWC",	"degC",	
                     "days",	"days",	"mm",	"VWC",	"degC",	"gCmd","µmol_CO2_m2_s",	"TF")
 
 for (col in names(column_units)) {
   Cham_USWKG_pulsedata[[col]] <- set_units(Cham_USWKG_pulsedata[[col]], column_units[[col]])
 }
  
 colnames(Cham_USWKG_pulsedata) <- newnames
 
 Cham_USWKG_pulsedata <- Cham_USWKG_pulsedata %>%
   mutate(Rsoil1 = ifelse(Rsoil1 < 0, NA, Rsoil1),
          Rsoil1 = ifelse(is.na(Rsoil1), NA, Rsoil1),
          Rsoil2 = ifelse(Rsoil2 < 0, NA, Rsoil2),
          Rsoil2 = ifelse(is.na(Rsoil2), NA, Rsoil2),
          Rsoil3 = ifelse(Rsoil3 < 0, NA, Rsoil3),
          Rsoil3 = ifelse(is.na(Rsoil3), NA, Rsoil3),
          Rsoil4 = ifelse(Rsoil4 < 0, NA, Rsoil4),
          Rsoil4 = ifelse(is.na(Rsoil4), NA, Rsoil4),
          Rsoil5 = ifelse(Rsoil5 < 0, NA, Rsoil5),
          Rsoil5 = ifelse(is.na(Rsoil5), NA, Rsoil5),
          Rsoil6 = ifelse(Rsoil6 < 0, NA, Rsoil6),
          Rsoil6 = ifelse(is.na(Rsoil6), NA, Rsoil6),
          Rsoil7 = ifelse(Rsoil7 < 0, NA, Rsoil7),
          Rsoil7 = ifelse(is.na(Rsoil7), NA, Rsoil7))
 
 Cham_USWKG_PULSETIME <- Cham_USWKG_pulsedata[Cham_USWKG_pulsedata$days_since_rain_event <= Cham_USWKG_pulsedata$max_pulse_duration, ]
 Cham_USWKG_NON_PULSETIME <- Cham_USWKG_pulsedata[Cham_USWKG_pulsedata$days_since_rain_event > Cham_USWKG_pulsedata$max_pulse_duration, ]
 
 ####
 ## Chamber 1
 ####
 
 #PULSETIME set up columns to pull to run the model for chamber 1
 Cham1 <- c("meanGPP", "Rsoil1", "VWC1", "Tsoil1")
 subset_Cham1P_holes <- Cham_USWKG_PULSETIME[, Cham1]
 complete_rows <- complete.cases(subset_Cham1P_holes)
 subset_Cham1P = subset_Cham1P_holes[complete_rows, ]
 
 #PULSETIME Assign variables
 meanSWC5_P <- subset_Cham1P$VWC1
 meanST5_P <- subset_Cham1P$Tsoil1
 meanGPP_P <- subset_Cham1P$meanGPP
 GPPmax_P <- max(subset_Cham1P$meanGPP)
 
 # Fit model
 Param_model4_P_CH1 <- nls(Rsoil1 ~ Fref*((meanGPP_P/GPPmax_P +n)/1+n) *
                          (1-c4*(0.1-meanSWC5_P)^2)*exp(b4*meanST5_P), 
                        data = subset_Cham1P,
                        start = list(Fref=0.75, c4=56.54, b4=0.04, n=0.84),
                        control = nls.control(maxiter = 1000, minFactor = 0.01)
 )
 Summary_Model4_P_CH1 = summary(Param_model4_P_CH1)
 estimates_P_CH1 <- coef(Summary_Model4_P_CH1)
 
 #NON-PULSETIME set up columns to pull to run the model for chamber 1
 subset_Cham1NP_holes <- Cham_USWKG_NON_PULSETIME[, Cham1]
 complete_rowsNP <- complete.cases(subset_Cham1NP_holes)
 subset_Cham1NP = subset_Cham1NP_holes[complete_rowsNP, ]
 
 #NON-PULSETIME Assign variables
 meanSWC5_NP <- subset_Cham1NP$VWC1
 meanST5_NP <- subset_Cham1NP$Tsoil1
 meanGPP_NP <- subset_Cham1NP$meanGPP
 GPPmax_NP <- max(subset_Cham1NP$meanGPP)
 
 
 # Fit model
 Param_model4_NP_CH1 <- nls(Rsoil1 ~ Fref*((meanGPP_NP/GPPmax_NP +n)/1+n) *
                             (1-c4*(0.1-meanSWC5_NP)^2)*exp(b4*meanST5_NP), 
                           data = subset_Cham1NP,
                           start = list(Fref=0.75, c4=56.54, b4=0.04, n=0.84),
                           control = nls.control(maxiter = 1000, minFactor = 0.01)
 )
 Summary_Model4_NP_CH1 = summary(Param_model4_NP_CH1)
 estimates_NP_CH1 <- coef(Summary_Model4_NP_CH1)
 
#### Chamber 1 summaries
 Summary_Model4_NP_CH1
 Summary_Model4_P_CH1
 
  # 
  # # Create a directory to save the PNG files
  # dir.create("KN_Chamber_plot")
  #   # Loop through variables and plot each as a time series
  # for (col in names(Cham_USWKG_pulsedata)[-1]) {
  #   p <- ggplot(Cham_USWKG_pulsedata, aes(x = date, y = .data[[col]])) +
  #     geom_line() +
  #     labs(title = paste0("KNCham_time_", col))
  #   print(p)
  # 
  #   # Save plot as PNG
  #   filename <- paste0("KN_Chamber_plot/", col, ".png")
  #   ggsave(filename, plot = p)
  # }
  # 
  # for (col in names(Cham_USWKG_pulsedata)[-1]) {
  #   p <- ggplot(Cham_USWKG_pulsedata, aes(x = date, y = .data[[col]])) +
  #     geom_boxplot() +
  #     labs(title = paste0("KNCham_BOX_", col))
  #   print(p)
  # 
  #   # Save plot as PNG
  #   filename <- paste0("KN_Chamber_plot/", col, ".png")
  #   ggsave(filename, plot = p)
  # }
  # 
  
  # Select the 7 variables you want to plot
  RsoilPlot <- c("meanRECO", "Rsoil1", "Rsoil2", "Rsoil3", "Rsoil4", "Rsoil5", "Rsoil6", "Rsoil7")
    VWCPlot <- c("meanSWC5", "VWC1", "VWC2", "VWC3", "VWC4", "VWC5", "VWC6", "VWC7")
    TsoilPlot <- c("meanST5", "Tsoil1", "Tsoil2", "Tsoil3", "Tsoil4", "Tsoil5", "Tsoil6", "Tsoil7")
  
    # Subset the dataframe to include only the selected variables
    subset_Rsoil <- Cham_USWKG_pulsedata[, RsoilPlot]
    subset_VWC <- Cham_USWKG_pulsedata[, VWCPlot]
    subset_Tsoil <- Cham_USWKG_pulsedata[, TsoilPlot]
    
    
    
    # Convert the dataframe to long format using tidyr
    Rsoil_long <- gather(subset_Rsoil, key = "Variable", value = "Value")
    
    # Plot the box plot for the selected variables
    ggplot(Rsoil_long, aes(x = Variable, y = Value)) +
      geom_boxplot(fill = "white", color = "black") +
      labs(title = "Box Plot for Selected Soil Respiration Variables plus RECO",
           x = "Measurement point", y = "Respiration (µmol CO2 m-2 s-1)") +
      theme_minimal()
    
    VWC_long <- gather(subset_VWC, key = "Variable", value = "Value")
    
  
    # Plot the box plot for the selected variables
    ggplot(VWC_long, aes(x = Variable, y = Value)) +
      geom_boxplot(fill = "white", color = "black") +
      labs(title = "Box Plot for Selected Soil Moisture Variables",
           x = "Measurement point", y = "Water content (VWC)") +
      theme_minimal()
    
    
    Tsoil_long <- gather(subset_Tsoil, key = "Variable", value = "Value")
    
    # Plot the box plot for the selected variables
    ggplot(Tsoil_long, aes(x = Variable, y = Value)) +
      geom_boxplot(fill = "white", color = "black") +
      labs(title = "Box Plot for Selected Tsoil Variables",
           x = "Measurement point", y = "Temperature (C)") +
      theme_minimal()
  
  

  # Plot the box plot for the selected variables
  
  subset_Rsoil_pulse <- Cham_USWKG_PULSETIME[, RsoilPlot]
  # Convert the dataframe to long format using tidyr
  Rsoil_longP <- gather(subset_Rsoil_pulse, key = "Variable", value = "Value")
  
  ggplot(Rsoil_longP, aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "white", color = "black") +
    labs(title = "Pulse time Box Plot for Selected Rsoil Variables",
         x = "Measurement point", y = "Respiration (µmol CO2 m-2 s-1)") +
    theme_minimal()+
    ylim(0, 10) +  # Set the y-axis limits
    theme(axis.title = element_text(size = 18),  # Adjust axis title size
          axis.text = element_text(size = 14))  # Adjust axis label size
   
  
  
  
  
  subset_Rsoil_NONpulse <- Cham_USWKG_NON_PULSETIME[, RsoilPlot]
  # Convert the dataframe to long format using tidyr
  Rsoil_longNP <- gather(subset_Rsoil_NONpulse, key = "Variable", value = "Value")
  
  ggplot(Rsoil_longNP, aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "white", color = "black") +
    labs(title = "Non-Pulse time Box Plot for Selected Rsoil Variables",
         x = "Measurement point", y = "Respiration (µmol CO2 m-2 s-1)") +
    theme_minimal()+
    ylim(0, 10) +  # Set the y-axis limits
    theme(axis.title = element_text(size = 18),  # Adjust axis title size
          axis.text = element_text(size = 14))  # Adjust axis label size
  
  
  subset_Rsoil_NONpulse_z_scores <- as.data.frame(lapply(subset_Rsoil_NONpulse, scale))
  
  
  # Convert the dataframe to long format using tidyr 
  long_Rsoil_NONpulse_z_scores <- gather(subset_Rsoil_NONpulse_z_scores, key = "Variable", value = "Value")
  
  ggplot(long_Rsoil_NONpulse_z_scores, aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "white", color = "black") +
    labs(title = "Non-Pulse time Box Plot for Selected Rsoil Variables",
         x = "Measurement point", y = "Respiration (µmol CO2 m-2 s-1)") +
    theme_minimal()+
    ylim(-10, 10) +  # Set the y-axis limits
    theme(axis.title = element_text(size = 18),  # Adjust axis title size
          axis.text = element_text(size = 14))  # Adjust axis label size
  
  # # Apply scale() to calculate z-scores for the selected columns
  # Rsoil_NONpulse_z_scores <- df
  # df_z_scores[selected_columns] <- lapply(df[selected_columns], scale, na.rm = TRUE)
  
  
  
  