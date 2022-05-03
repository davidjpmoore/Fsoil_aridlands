#4/13/2022
#Anastasia Makhnykina
#grab data we need


#Load Datarain
datarain_pro=read.csv("data/datarain_processed.csv", header=TRUE, na.strings = "NaN")
#remove unnessessary columns


Pulse5=read.csv("data/Pulse5.csv", header=TRUE, na.strings = "NaN")
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
formattable((head(Pulse5)))

#Describing our data - data set "Pulse 1 = 5th Pulse from the list" I have to Rename it everywhere!

Pulse5 %>%
  diagnose(RECO)

diagnose_numeric(Pulse5)

formattable(diagnose_numeric(Pulse5))
summary(Pulse5)

summary(Pulse5$AT2)
summary(Pulse5$r)
summary(Pulse5$RECO)

hist.default(Pulse5$AT2, col='gray')

hist.default(Pulse5$NEE, col='gray')

hist.default(Pulse5$r, col='gray')

hist.default(Pulse5$RECO, col='gray')
table(Pulse5$RECO)

plot(Pulse5$GPP~Pulse5$RECO, col='blue')

# T-test shows the statistical inferences and the confidence interval .as outcomes.
# The p-value is the probability value significant to the null hypothesis. 
# And the percentage value is the confidence interval.

t.test(Pulse5$RECO)

var(Pulse5$RECO)

plot(Pulse5$data_time_Start,Pulse5$RECO, col='blue')

ggplot(mapping=aes(x=Pulse5$dateStart,y=Pulse5$RECO))+
  geom_point()

ggplot(mapping=aes(x=Pulse5$NEE,y=Pulse5$RECO))+
  geom_point()

ggplot(mapping=aes(x=Pulse5$NEE,y=Pulse5$RECO))+
  geom_point()

ggplot(Pulse5, aes(x=Pulse5$DOY_S))+
  geom_point(aes(y=Pulse5$RECO, color="red"))+
  geom_point(aes(y=Pulse5$SWC5, color="blue"))



ggplot(Pulse5, aes(x=Pulse5$DOY_S, Pulse5$RECO))+
  geom_point(aes(y=Pulse5$RECO, color="red"))+
  geom_point(aes(y=Pulse5$SWC5, color="blue"))+
  scale_y_continuous(
    name="Reco (micromol CO2 m-2 s-1)",
    sec.axis=sec_axis(~., name="SWC 5 cm (%)")
  )

#Some statistical analysis for the numeric data

head(Pulse5)

numeric_Pulse5 <- diagnose_numeric(Pulse5)


numeric_Pulse5 <- as.numeric(as.double(Pulse5))


#Making the correlation matrix 

cormatrix <- cor(Pulse5[sapply(Pulse5, is.numeric)])

corrplot(cormatrix)


#1 - Choose the columns which I really need for analysis
#2 - Make the analysis of rain events based on the previous conditions and look at the variables directions after the rain OR pulse
#3 - Which rain can be qualified as pulse??? - more then 5 mm per day!


Pulse5_pro <- select(Pulse5, c(PA, AT2, RH2, AT6, RH6, r, SWC5, 
                               SWC15, SWC30, SWC30, ST5, 
                               ST15,ST30, NEE, RECO, GPP, high_precip, 
                               RainEvent, dateStart, dateEnd, data_time_Start,
                               data_time_End, DOY_S, DOY_E, Rain_DOY, sum_rain))


plot(Pulse5_pro$SWC5)

#Make this graph without 0-values
plot(Pulse5_pro$r) 

Pulse5 %>%
  filter(sum_rain>0) %>%
  ggplot(aes(x=SWC5, y=RECO, color=RainEvent))+
  geom_point()

Pulse5 %>%
  filter(sum_rain==0) %>%
  ggplot(aes(x=SWC5, y=RECO, color=DOY_S))+
  geom_point()

Pulse5_pro %>%
  filter(DOY_S<172)%>%
  ggplot(aes(x=SWC5, y=RECO))+
  geom_point()

plot(Pulse5_pro$SWC5)

Pulse5_pro%>%
  na.omit()%>%
  summarise(mean=mean(RECO), sd=sd(RECO),
            n=n(),
            stderr=sd/sqrt(n)) %>%
  ggplot(aes(x=DOY_S, y=mean)) + 
  geom_point()+
  geom_errorbar(aes(x=DOY_S, ymin=mean - stderr, ymax=mean + stderr))+
  xlab('DOY') +
  ylab('Mean Reco')
               


Pulse5 %>%
  ggplot(aes(x=DOY_S, y=SWC5))+
  geom_point()

Pulse5_pro %>%
  filter(DOY_S<172)%>%
  summarise(mean(SWC5))
  
  
Pulse5_pro %>%
  filter(DOY_S>=172)%>%
  summarise(mean(SWC5))
  

Pulse5_pro %>%
  filter(DOY_S<172)%>%
  summarise(mean(SWC5))


Pulse5_pro %>%
  filter(DOY_S %in% (175:183))%>%
  summarise(mean(SWC5))


Pulse5_pro %>%
  filter(DOY_S==178)%>%
  summarise(mean(SWC5))


Pulse5_pro <- Pulse5_pro %>%
  group_by(DOY_S) %>%
  mutate(mean_SWC = mean(SWC5))

plot(Pulse5_pro$DOY_S, Pulse5_pro$RECO)

Pulse5_pro %>%
  filter(DOY_S==176)%>%
  dplyr :: summarise(mean=colMeans(Pulse5_pro[, c(7,8,9)]))

num_cols <- lapply(Pulse5_pro, is.numeric)

Pulse5_pro %>%
  filter(DOY_S<176) %>%
  summarise(mean=colMeans(Pulse5_pro[, c(1:15, 25)]))

Pulse5_pro %>%
  filter(DOY_S<176) %>%
  summarise(rowMeans(Pulse5_pro[, c(1:15, 25)]))


mydata <- Pulse5_pro[, c(1,2,3,4,5,7,8,9,10,11,12,13,14,15)]

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


# Write sum Pulse 5
write.csv(file="data/Pulse5_sum.csv", datarain)

Pulse5_sum <- Pulse5_pro %>%
  group_by(as.numeric(DOY_S)) %>%
    dplyr:: summarise(meanAT2=mean(AT2,na.rm=TRUE), 
                      meanAT6=mean(AT6, na.rm=TRUE),
                      sum_R=mean(sum_rain, na.rm=TRUE),
                      rain_events=sum(RainEvent, na.rm=TRUE),
                      meanRH2=mean(RH2, na.rm=TRUE),
                      meanRH6=mean(RH6,na.rm=TRUE),
                      meanSWC5=mean(SWC5, na.rm=TRUE),
                      meanSWC15=mean(SWC15, na.rm=TRUE),
                      meanSWC30=mean(SWC30, na.rm=TRUE), 
                      meanST5=mean(ST5, na.rm=TRUE),
                      meanST15=mean(ST15, na.rm=TRUE),
                      meanST30=mean(ST30, na.rm=TRUE),
                      meanNEE=mean(NEE, na.rm=TRUE), 
                      meanGPP=mean(GPP, na.rm=TRUE),
                      meanRECO=mean(RECO, na.rm=TRUE))
Pulse5_pro %>%
  na.omit() %>%
  dplyr:: summarise(mean=mean(RECO), sd=sd(RECO),
            n=n(),
            stderr=sd/sqrt(n)) %>% 
  ggplot(aes(x=DOY_S, y=mean)) + 
  geom_point()+
  geom_errorbar(aes(x=DOY_S, ymin=mean - stderr, ymax=mean + stderr))+
  xlab('DOY') +
  ylab('Mean Reco')


#2 - Make the analysis of 2 rain events based on the previous conditions 

pdata = read.csv("data/Pulse5_sum.csv", header=TRUE, na.strings = "NaN")

p1.corr.df = as.data.frame(matrix(nrow = 7, ncol = 14))
p2.corr.df = as.data.frame(matrix(nrow = 7, ncol = 14))

cols = c('Precip_event','Days_before_event',
         'meanAT2','meanAT6','meanRH2','meanRH6','meanSWC5','meanSWC15','meanSWC30','meanST5','meanST15','meanST30','meanNEE','meanGPP')

vars = cols[3:length(cols)]

colnames(p1.corr.df) = cols
colnames(p2.corr.df) = cols

p1.corr.df$Precip_event = c(rep('P1',7))
p1.corr.df$Days_before_event = c(seq(1,7,1))
p2.corr.df$Precip_event = c(rep('P2',7))
p2.corr.df$Days_before_event = c(seq(1,7,1))


P1day = 8
P2day = 16

for (i in 1 : 7){
  # Precip event 1
  p1.corr.df$meanAT2[i] = cor(x = pdata$meanAT2[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanAT6[i] = cor(x = pdata$meanAT6[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanRH2[i] = cor(x = pdata$meanRH2[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanRH6[i] = cor(x = pdata$meanRH6[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanSWC5[i] = cor(x = pdata$meanSWC5[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanSWC15[i] = cor(x = pdata$meanSWC15[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanSWC30[i] = cor(x = pdata$meanSWC30[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanST5[i] = cor(x = pdata$meanST5[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanST15[i] = cor(x = pdata$meanST15[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanST30[i] = cor(x = pdata$meanST30[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanNEE[i] = cor(x = pdata$meanNEE[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  p1.corr.df$meanGPP[i] = cor(x = pdata$meanGPP[P1day:(P1day-i)], y = pdata$meanRECO[P1day:(P1day-i)], method = 'pearson')
  
  # Precip event 2
  p2.corr.df$meanAT2[i] = cor(x = pdata$meanAT2[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanAT6[i] = cor(x = pdata$meanAT6[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanRH2[i] = cor(x = pdata$meanRH2[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanRH6[i] = cor(x = pdata$meanRH6[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanSWC5[i] = cor(x = pdata$meanSWC5[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanSWC15[i] = cor(x = pdata$meanSWC15[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanSWC30[i] = cor(x = pdata$meanSWC30[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanST5[i] = cor(x = pdata$meanST5[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanST15[i] = cor(x = pdata$meanST15[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanST30[i] = cor(x = pdata$meanST30[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanNEE[i] = cor(x = pdata$meanNEE[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
  p2.corr.df$meanGPP[i] = cor(x = pdata$meanGPP[P2day:(P2day-i)], y = pdata$meanRECO[P2day:(P2day-i)], method = 'pearson')
}

library(reshape2)
p1.corr.df = melt(p1.corr.df, id.vars = c('Precip_event','Days_before_event'), variable.name = 'Var', value.name = 'R')
p2.corr.df = melt(p2.corr.df, id.vars = c('Precip_event','Days_before_event'), variable.name = 'Var', value.name = 'R')

corr.df = rbind(p1.corr.df, p2.corr.df)
corr.df = subset(corr.df, Days_before_event != 1)

# ---- Plot data
library(ggplot2)
library(colorRamps)

bluecols = colorRampPalette(c('lightblue','navyblue'))

ggplot(corr.df, aes(x = 1, y = R, fill = Var)) +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black') +
  scale_fill_manual(values = bluecols(12)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,0.25)) +
  facet_grid(Precip_event ~ Days_before_event) +
  xlab('Number of days prior to precipitation event used to compute R') +
  ylab('Correlation (Pearson R)') +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank())







