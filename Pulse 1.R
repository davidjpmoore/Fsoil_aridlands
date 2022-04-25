#4/13/2022
#Anastasia Makhnykina
#grab data we need


#Load Datarain
datarain_pro=read.csv("data/datarain_processed.csv", header=TRUE, na.strings = "NaN")
#remove unnessessary columns


Pulse1=read.csv("data/Pulse1.csv", header=TRUE, na.strings = "NaN")
#remove unnessessary columns


#packages we need
library(dplyr)
library(tidyverse)
library(lubridate)
library(skimr)
library(data.table)
library(corrplot)
library(scales)
library(PerformanceAnalytics)
library(xtable)
library(Hmisc)
library(ggpubr)


# Required package for quick package downloading and loading 
#install.packages("pacman")

# Downloads and load required packages
pacman::p_load(dlookr,
               formattable,
               ggdist,
               ggpubr,
               ggridges,
               kableExtra,
               knitr,
               papeR,
               RColorBrewer,
               Stat2Data,
               tidyverse)

# DATA ANALYSIS
formattable((head(Pulse1)))

#Describing our data

Pulse1 %>%
  diagnose(RECO)

formattable(diagnose_numeric(Pulse1))
summary(Pulse1)

summary(Pulse1$AT2)
summary(Pulse1$r)
summary(Pulse1$RECO)

hist.default(Pulse1$AT2, col='gray')

hist.default(Pulse1$NEE, col='gray')

hist.default(Pulse1$r, col='gray')

hist.default(Pulse1$RECO, col='gray')
table(Pulse1$RECO)

plot(Pulse1$GPP~Pulse1$RECO, col='blue')

# T-test shows the statistical inferences and the confidence interval .as outcomes.
# The p-value is the probability value significant to the null hypothesis. 
# And the percentage value is the confidence interval.

t.test(Pulse1$RECO)

var(Pulse1$RECO)

plot(Pulse1$data_time_Start,Pulse1$RECO, col='blue')

ggplot(mapping=aes(x=Pulse1$dateStart,y=Pulse1$RECO))+
  geom_point()

ggplot(mapping=aes(x=Pulse1$NEE,y=Pulse1$RECO))+
  geom_point()

ggplot(mapping=aes(x=Pulse1$NEE,y=Pulse1$RECO))+
  geom_point()

ggplot(Pulse1, aes(x=Pulse1$DOY_S))+
  geom_point(aes(y=Pulse1$RECO, color="red"))+
  geom_point(aes(y=Pulse1$SWC5, color="blue"))



ggplot(Pulse1, aes(x=Pulse1$DOY_S, Pulse1$RECO))+
  geom_point(aes(y=Pulse1$RECO, color="red"))+
  geom_point(aes(y=Pulse1$SWC5, color="blue"))+
  scale_y_continuous(
    name="Reco (micromol CO2 m-2 s-1)",
    sec.axis=sec_axis(~., name="SWC 5 cm (%)")
  )

#Some statistical analysis for the numeric data

head(Pulse1)

numeric_Pulse1 <- diagnose_numeric(Pulse1)


#Making the correlation matrix 

cormatrix <- cor(Pulse1[sapply(Pulse1, is.numeric)])

corrplot(cormatrix)


#1 - Choose the columns which I really need for analysis
#2 - Make the analysis of rain events based on the previous conditions and look at the variables directions after the rain OR pulse
#3 - Which rain can be qualified as pulse??? 


Pulse1_pro <- select(Pulse1, c(PA, AT2, RH2, AT6, RH6, r, SWC5, 
                               SWC15, SWC30, SWC30, ST5, 
                               ST15,ST30, NEE, RECO, GPP, high_precip, 
                               RainEvent, dateStart, dateEnd, data_time_Start,
                               data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain))


plot(Pulse1_pro$SWC5)

#Make this graph without 0-values
plot(Pulse1_pro$r) 

Pulse1 %>%
  filter(sum_rain>0) %>%
  ggplot(aes(x=SWC5, y=RECO, color=RainEvent))+
  geom_point()

Pulse1 %>%
  filter(sum_rain==0) %>%
  ggplot(aes(x=SWC5, y=RECO, color=DOY_S))+
  geom_point()

Pulse1_pro %>%
  filter(DOY_S<172)%>%
  ggplot(aes(x=SWC5, y=RECO))+
  geom_point()

plot(Pulse1_pro$SWC5)

Pulse1 %>%
  ggplot(aes(x=DOY_S, y=SWC5))+
  geom_point()

Pulse1_pro %>%
  filter(DOY_S<172)%>%
  summarise(mean(SWC5))
  
  
Pulse1_pro %>%
  filter(DOY_S>=172)%>%
  summarise(mean(SWC5))
  

Pulse1_pro %>%
  filter(DOY_S<172)%>%
  summarise(mean(SWC5))


Pulse1_pro %>%
  filter(DOY_S %in% (175:183))%>%
  summarise(mean(SWC5))


Pulse1_pro %>%
  filter(DOY_S==178)%>%
  summarise(mean(SWC5))


Pulse1_pro <- Pulse1_pro %>%
  group_by(DOY_S) %>%
  mutate(mean_SWC5 = mean(SWC5))

plot(Pulse1_pro$DOY_S, Pulse1_pro$RECO)

Pulse1_pro %>%
  filter(DOY_S==176)%>%
  summarise(mean=colMeans(Pulse1_pro[, c(7,8,9)]))

num_cols <- lapply(Pulse1_pro, is.numeric)

Pulse1_pro %>%
  filter(DOY_S<176) %>%
  summarise(mean=colMeans(Pulse1_pro[, c(1:15, 25)]))

Pulse1_pro %>%
  filter(DOY_S<176) %>%
  summarise(rowMeans(Pulse1_pro[, c(1:15, 25)]))


mydata <- Pulse1_pro[, c(1,2,3,4,5,7,8,9,10,11,12,13,14,15)]

corr.table.function = function(mydata){
  
  corr.mat = rcorr(as.matrix(mydata), type = 'pearson')
  
  R = corr.mat$r
  p = corr.mat$P
  
  mystars = ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  R = format(round(cbind(rep(-1.11, ncol(mydata)), R), 2))[,-1]
  
  Rnew = matrix(paste(R, mystars, sep=""), ncol=ncol(mydata))
  diag(Rnew) = paste(diag(R), " ", sep="")
  rownames(Rnew) = colnames(mydata)
  colnames(Rnew) = paste(colnames(mydata), "", sep="")
  
  Rnew = as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew = Rnew[,1:(ncol(Rnew)-1)]
  Rnew = Rnew[2:nrow(Rnew),]
  Rnew = as.data.frame(Rnew)
  
  
  corr.table = ggtexttable(Rnew, theme = ttheme())
  corr.table
}

corr.table.function(mydata)



mcor <- round(cor(mydata),2)
mcor
upper.tri(mcor)

# Hide upper triangle
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper

print(xtable(upper), type="html")

corstars(mydata[,1:14], result="html")

chart.Correlation(mydata, histogram=TRUE, pch=19)

Pulse1_sum <- Pulse1_pro %>%
  group_by(DOY_S) %>%
  summarise(meanAT2=mean(AT2,na.rm=TRUE), 
            meanAT6=mean(AT6, na.rm=TRUE),
            sum_R=mean(sum_rain, na.rm=TRUE),
            rain_events=sum(RainEvent, na.rm=TRUE),
            meanRH2=mean(RH2, na.rm=TRUE),
            meanRH6=mean(RH6,na.rm=TRUE),
            meanSWC5=mean(SWC5, na.rm=TRUE), sdSWC5=sd(SWC5, na.rm=TRUE),
            meanSWC15=mean(SWC15, na.rm=TRUE),
            meanSWC30=mean(SWC30, na.rm=TRUE), 
            meanST5=mean(ST5, na.rm=TRUE),
            meanST15=mean(ST15, na.rm=TRUE),
            meanST30=mean(ST30, na.rm=TRUE),
            meanNEE=mean(NEE, na.rm=TRUE), 
            meanGPP=mean(GPP, na.rm=TRUE),
            meanRECO=mean(RECO, na.rm=TRUE), sdReco=sd(RECO, na.rm=TRUE))


Pulse1_sum %>%
  ggplot(aes(x=DOY_S, y=meanRECO))+
  geom_point()

Pulse1_sum %>%
  ggplot(aes(x=DOY_S, y=meanRECO)) + 
  geom_point()+
  geom_errorbar(aes(ymin=meanRECO - sdReco, ymax=meanRECO + sdReco),
                width=.8, position=position_dodge(0.05))


Pulse1_sum %>%
  ggplot(aes(x=DOY_S, y=meanSWC5)) + 
  geom_point()+
  geom_errorbar(aes(ymin=meanSWC5 - sdSWC5, ymax=meanSWC5 + sdSWC5),
                width=.8, position=position_dodge(0.05))

#2 - Make the analysis of rain events based on the previous conditions and look at the variables directions
# after the rain OR pulse






