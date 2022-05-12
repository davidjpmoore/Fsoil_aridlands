
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(tidyr)
library(ggplot2)

#  ########################################################### 
# # READ DAILY VWC data unless already created
# # file="data/VWC_daily.csv"
#  ###########################################################
VWC_slim_RM_HHly <- read.csv("data/Halfhourly_VWC_Weather_112721.csv",
                              sep=",",header=TRUE, na.strings = "NA")
VWC_slim_RM_HHly$Date10 = as.Date(substr(VWC_slim_RM_HHly$TIMESTAMP, 1,10))
#  ###########################################################

#using pivot_longer to change columns to rows
VWC_slim_RM_HHly_slim <-
  pivot_longer(data= VWC_slim_RM_HHly,
               cols = starts_with("VWC"),
               names_to = "Var_Plot_Prb_Hse",
               values_to = "VWC_prop"
  )

VWC_slim_RM_HHly_slim <- VWC_slim_RM_HHly_slim %>% 
  separate(Var_Plot_Prb_Hse, c('Variable', 'Plot', 'Probe', 'House'), sep = "_")

VWC_slim_RM_HHly_slim <-  VWC_slim_RM_HHly_slim %>% 
  mutate(Plot = recode(Plot, 'P1' = '01', 'P2' = '02', 
                       'P3' = '03', 'P4' = '04', 'P5' = '05','P6' = '06', 
                       'P7' = '07', 'P8' = '08', 'P9' = '09', 
                       'P10' = '10', 'P11' = '11', 'P12' = '12'))  %>%
  mutate(House = recode(House, 'H1' = '01', 'H2' = '02', 
                        'H3' = '03', 'H4' = '04', 'H5' = '05')) %>%
  mutate(House_plot = paste(House, Plot, sep="_"))

#READ IN TREATMENT KEY

TRT_RM_mge <- read.csv("data/TreatmentKey_tidy_RM.csv",
                             sep=",",header=TRUE, na.strings = "NA")

VWC_slim_RM_HHly_slim_TRT =  full_join( TRT_RM_mge,VWC_slim_RM_HHly_slim,
                         by = c("House_plot" = "House_plot"))


#calculate average by day

VWC_slim_RM_TRT_daily =VWC_slim_RM_HHly_slim_TRT %>%
  group_by(Date10, House_plot, Summer) %>% 
  summarize(
    VWC_daily = sum(VWC_prop, na.rm=TRUE),
    Tout_daily = mean(T_outside, na.rm=TRUE), 
    RHout_daily = mean(RH_outside, na.rm=TRUE),
    T_in_daily = mean(T_inside, na.rm=TRUE),
    RHin_daily = mean(RH_inside, na.rm=TRUE)
    )


VWC_slim_RM_TRT_daily

write_csv(VWC_slim_RM_TRT_daily, file="data/RM_VWC_slim_byTRT_daily.csv")


VWC_daily_byTreat_mge <- ggplot(VWC_slim_RM_TRT_daily, 
                                aes(x=Date10, y=VWC_daily,
                                    group=Summer, 
                                    color=Summer
                                )) +
  theme_bw() +
  theme(axis.ticks.length = unit(-0.2, "cm"))+
  #Note that I have stored the Date10 variable as.Date 
  #this allows me to use date functions like this
  
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y")+
  xlab("Date") +
  ylab("Volumetric Water Content (prop)") +
  geom_point() 
