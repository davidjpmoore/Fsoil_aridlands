#Purpose: To check the revised data that Russ Scott sent on Sept 15th
#Author: Dave Moore 
#Date 10/19/2022

#Data2018_sent09152022 = read.csv(file="data/AddedPartionedCflux_US-Wkg_HH_201712312330_201812312330.csv")

Data2018_sent102022 = read.csv(file="data/AddedPartionedCflux_US-Wkg_HH_201712312330_201812312330.csv")

plot(Data2018_sent102022$H2O[Data2018_sent102022$H2O>-1])
plot(Data2018_sent102022$T_SONIC[Data2018_sent102022$T_SONIC>-1])
  plot(Data2018_sent102022$RECO[Data2018_sent102022$RECO>-1])
  
  
  plot(Data2018_sent102022$SWC_1_1_1[Data2018_sent102022$SWC_1_1_1>-1])
    plot(Data2018_sent102022$SWC_1_2_1[Data2018_sent102022$SWC_1_2_1>-1])
    plot(Data2018_sent102022$SWC_1_3_1[Data2018_sent102022$SWC_1_3_1>-1])