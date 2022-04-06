#load Kendall data in R
datarainman=read.csv("data/kendall2017.csv", header=TRUE, na.strings = "NaN", sep=";")
dataGPP=read.csv("data/GPP_data.csv", header=TRUE, sep=";")

plot(dataGPP$GPP)

newGPP = dataGPP$GPP*5 -3

dataGPP$newGPP =  dataGPP$GPP*5 -3

#plot two dimensional graph
plot(dataGPP22$GPP,dataGPP22$ER.ecosystem.respiration)

#make correlation
cor(dataGPP22$NEE,dataGPP22$ER.ecosystem.respiration,use="everything", method=c("pearson"))

#make ggplot with dots
ggplot(data=dataGPP22)+geom_point(mapping=aes(x=NEE,y=ER.ecosystem.respiration))

# summary of your dataset
summary(dataGPP22)

#ggplot in other way
ggplot(dataeddy, aes(x=Fs_1,y=Ts_1))

#add new column as a function
mutate(c,Tsall=Ts_1+TFs_2+TFs_3+Ts_4)

#change directory
setwd("C:/Users/sunlife1408/Documents/RainMan2022")

#open cvs
rains=read.csv("data/20162017Meteo.csv", header=TRUE, na.strings = "NaN", sep=";")

# read precip. column from dataset
p = X20162017Meteo$P
#p[p == 0] = NA


#mean value 
pmean = mean(p ,na.rm = TRUE)

#sd value
psd = sd(p, na.rm = TRUE)


#data type
typeof(rains$P)

#change data type
rains$P<- as.numeric(as.double(rains$P))

#normal ditribution
dist=dnorm(p,mean=pmean,sd=psd)
plot(dist, type = 'l')

x = p
y = dist

plot(x, y,
     xlab = 'Half-Hourly Precipitation [mm]',
     ylab = 'Normal Distribution')

hist(p, breaks = 50,
     xlab = 'Half-Hourly Precipitation [mm]')

#simple filtering
filter(X20162017Meteo,P>0)

#substr
#to parse bits of strings
# year_1=substr(X20162017Meteo$TIMESTAMP_START,1,4)
YEARTEST1=substr(X20162017Meteo$TIMESTAMP_START,1,4)
HIGHPRECIP= filter(X20162017Meteo, p>6)
plot(HIGHPRECIP$TIMESTAMP_START, HIGHPRECIP$P)


#paste or past0
#to add different columns together
#paste("one", "two", sep="")
#paste("one", "two", sep=",")
