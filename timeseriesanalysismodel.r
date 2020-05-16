rm(list=ls())
#setwd("D:\\Big Data/2000-16-traffic-flow-england-scotland-wales")

#install.packages('dplyr')
#install.packages('zoo')
library('dplyr')
library('zoo')
require(forecast)
require(TSPred)

preprocess <- function(filename)
{
  dfms <- read.csv(filename)
  dfms
  dfms <- subset(dfms, select = -c(Location_Easting_OSGR,Location_Northing_OSGR,Police_Force,Junction_Detail,LSOA_of_Accident_Location))
  dfms[dfms == ""] <- NA
  dfms$Junction_Control[is.na(dfms$Junction_Control)] <- ""
  dfms[is.na(dfms[,"Junction_Control"])] <- ""
  dfms <- na.omit(dfms)
  dfms$Date <- as.Date(dfms$Date,"%d/%m/%Y")
  y<-factor(dfms$Junction_Control)
  return (dfms)
}

TimingData <- function(dfm)
{
  timeseriesData <- dfm %>%
    mutate(month = format(dfm$Date, "%m"), year = format(dfm$Date, "%y"))%>%
    group_by(month, year) %>%
    summarise(n())
  timeseriesData <- timeseriesData[with(timeseriesData,order(year)),]
  timeseriesData$Date <- as.yearmon(paste(timeseriesData$year, timeseriesData$month), "%Y %m")
  within(timeseriesData, Date <- sprintf("%s-%02s", year, month))
  timeseriesData <- subset(timeseriesData, select = -c(year, month))
  timeseriesData <- as.data.frame(timeseriesData)
  return(timeseriesData)
  
}

dfm1 <- preprocess('accidents_2005_to_2007.csv')
dfm2 <-  preprocess('accidents_2009_to_2011.csv')
dfm3 <- merge(dfm1,dfm2,all=T)
timeseriesData <- TimingData(dfm3)
testsetdfm <- preprocess('accidents_2012_to_2014.csv')
testsetSeries <- TimingData(testsetdfm)
testsetSeries
data2 <- ts(timeseriesData[,1], start = c(2005,1), frequency = 12)
data2

data3 <- ts(testsetSeries[,1], start = c(2012,1), frequency = 12)
data3

testsetSeriesmodified <-testsetSeries[13:36,]
data3modified <-  ts(testsetSeriesmodified[,1], start = c(2013,1), frequency = 12)
data3modified

## Training Set
plot(data2, xlab="Years", ylab = "Number of Accidents")
plot(stl(data2, "periodic" ))

## force variance to be constant
plot(log10(data2), xlab="Years", ylab = "Number of Accidents")
plot(stl(log10(data2), "periodic") )

plot(diff(data2), xlab="Years", ylab = "Number of Accidents")
plot(stl(diff(data2), "periodic") )

##Test Set
plot(data3, xlab="Years", ylab = "Number of Accidents")
plot(data3modified, xlab="Years", ylab = "Number of Accidents")

######### Model #####
ARIMAfit <- auto.arima((log10(data2)), approximation=FALSE, trace=TRUE, allowdrift=F)
summary(ARIMAfit)
pred <- predict(ARIMAfit, n.ahead = 48)

######## Predictions #################
plot((data3),type="l",xlim=c(2012,2015), ylim=c(8000,18000), xlab = "Year",ylab = "Number of Accidents")
#plot the prediction and its +- 2 std dev. (expected error)
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+1*pred$se),col="orange")
lines(10^(pred$pred-1*pred$se),col="orange")

accuracy(10^(pred$pred),data3)

############################# Prediction after removing 2012 ##############
plot((data3modified),type="l",xlim=c(2013,2015), ylim=c(8000,18000), xlab = "Year",ylab = "Number of Accidents")
#plot the prediction and its +- 2 std dev. (expected error)
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+1*pred$se),col="orange")
lines(10^(pred$pred-1*pred$se),col="orange")

accuracy(10^(pred$pred),data3modified)

############### Training set accuracy #############
accuracy(ARIMAfit)
residuals(ARIMAfit)
plot(residuals(ARIMAfit))
