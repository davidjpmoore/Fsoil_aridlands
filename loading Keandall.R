#load Kendall data in R
datarainman=read.csv("RainMan 2022/kendall2017.csv", header=TRUE, na.strings = "NaN", sep=";")
dataGPP=read.csv("RainMan 2022/GPP_data.csv", header=TRUE, sep=";")

plot(dataGPP$GPP)

newGPP = dataGPP$GPP*5 -3

dataGPP$newGPP =  dataGPP$GPP*5 -3


# 
# RsoilWkg<- read.csv(file="RainMan 2022/kendall2017dos.csv", sep=";", header=TRUE, skip =0,  
#                     comment.char = "",check.names = FALSE, quote="",
#                     na.strings=c("NA","NaN", " ") )