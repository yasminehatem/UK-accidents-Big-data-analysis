
install.packages('ggplot2')
install.packages("e1071")
install.packages("caret")
install.packages("data.table")
install.packages(("nnet"))
install.packages("randomForest")
rm(list=ls())
ls()
library(nnet)
library("e1071")
library("caret")
library(ggplot2)
library(data.table)
library(randomForest)
require(caTools)
############### #################### Dina preprocessing 2009-2011

dfm <- read.csv("accidents_2009_to_2011.csv")
#summary(acc_data)
names(dfm) #show clomuns names
str(dfm)
head(dfm,1)

dfm <- subset(dfm, select = -c(Location_Easting_OSGR,Location_Northing_OSGR,Police_Force,Junction_Detail,LSOA_of_Accident_Location))
str(dfm)
names(dfm)
dfm[dfm == ""] <- NA 
dfm$Junction_Control[is.na(dfm$Junction_Control)] <- ""
dfm[is.na(dfm[,"Junction_Control"])] <- ""
dfm <- na.omit(dfm)
str(dfm)
y<-factor(dfm$Junction_Control)
y
nrow(dfm)

############### #################### Dina preprocessing 2005-2007
dfm2 <- read.csv("accidents_2005_to_2007.csv")
#summary(acc_data)
names(dfm2) #show clomuns names
str(dfm2)
head(dfm2,1)
nrow(dfm2)

dfm2 <- subset(dfm2, select = -c(Location_Easting_OSGR,Location_Northing_OSGR,Police_Force,Junction_Detail,LSOA_of_Accident_Location))
str(dfm2)
names(dfm2)
dfm2[dfm2 == ""] <- NA 
dfm2$Junction_Control[is.na(dfm2$Junction_Control)] <- ""
dfm2[is.na(dfm2[,"Junction_Control"])] <- ""
dfm2 <- na.omit(dfm2)
str(dfm2)
y<-factor(dfm2$Junction_Control)
y
nrow(dfm2)
#####################
##### Dina preprocessing 2012-2014
dfm3 <- read.csv("accidents_2012_to_2014.csv")
#summary(acc_data)
names(dfm3) #show clomuns names
str(dfm3)
head(dfm3,1)

dfm3 <- subset(dfm3, select = -c(Location_Easting_OSGR,Location_Northing_OSGR,Police_Force,Junction_Detail,LSOA_of_Accident_Location))
str(dfm3)
names(dfm3)
dfm3[dfm3 == ""] <- NA 
dfm3$Junction_Control[is.na(dfm3$Junction_Control)] <- ""
dfm3[is.na(dfm3[,"Junction_Control"])] <- ""
dfm3 <- na.omit(dfm3)
str(dfm3)
y<-factor(dfm3$Junction_Control)
y
nrow(dfm3)
############# studyin attributes to decide model
table(dfm$Speed_limit)  #categorial
table(dfm$Number_of_Vehicles)
table(dfm$Number_of_Casualties)
table(dfm$Junction_Control)
table(dfm$Local_Authority_.District.)
table(dfm$Local_Authority_.Highway.)
length(table(dfm$Junction_Control))
table(dfm$Time)
table(dfm$Pedestrian_Crossing.Human_Control)
table(dfm$Pedestrian_Crossing.Physical_Facilities)
table(dfm$Light_Conditions)
length(table(dfm$Light_Conditions))
table(dfm$Urban_or_Rural_Area)
length(table(dfm$Urban_or_Rural_Area))

length(table(dfm$Weather_Conditions))
table(dfm$Road_Surface_Conditions)
length(table(dfm$Road_Surface_Conditions))
table(dfm$Special_Conditions_at_Site) #outlier awe
table(dfm$Carriageway_Hazards) #? outliers

#reconstruting a dataset to model the severity

#data3bind 3 datasets from 2005 to 2014 binded with all severities 1 2 3 
data3bind<-rbind(dfm,dfm2,dfm3)
set.seed(42)  #to mix and reorder  randomly because they were binded in order
rows<-sample(nrow(data3bind))
data3bind<-data3bind[rows,]
head(data3bind,20)

#data three bind has only the columns needed to fit the model
data3bind=subset(data3bind, select = c(Accident_Severity,Number_of_Vehicles
                                       ,Day_of_Week,X1st_Road_Class,Road_Type,Speed_limit,
                                       X2nd_Road_Class
                                       ,Road_Surface_Conditions))

x1<-subset(data3bind, Accident_Severity%in% c(1))
nrow(x1) #3adad el severity 1 f kol el data 19415



x2<-subset(data3bind, Accident_Severity%in% c(2))
nrow(x2)  #3adad el severity 2 f kol el data 204119

x3<-subset(data3bind, Accident_Severity%in% c(3))
nrow(x3) #3adad el severity 3 f kol el data 1275966


x1new<-x1

x2new <- x2[-sample(1:nrow(x2),154119 ), ] #delete random rows where severity =2
#x2new<-x2
nrow(x2new) #3adad el severity 2 f kol el data ba2a

x3new <- x3[-sample(1:nrow(x3),1175966), ] #delete random rows where severity =3
nrow(x3new) #3adad el severity 3 f kol el data ba2a


mydatamodel<-rbind(x1new,x2new,x3new)
head(mydatamodel,20)
nrow(mydatamodel)
set.seed(42)
rows<-sample(nrow(mydatamodel))
mydatamodel<-mydatamodel[rows,]
head(mydatamodel,20)

###################################################################### Data splitting for model
indxTrain <- createDataPartition(y = mydatamodel$Accident_Severity,p = 0.75,list = FALSE)
training <- mydatamodel[indxTrain,]
head(training,1)
testing <- mydatamodel[-indxTrain,]
names(testing)
head(testing,1)
prop.table(table(mydatamodel$Accident_Severity)) * 100
prop.table(table(training$Accident_Severity)) * 100
prop.table(table(testing$Accident_Severity)) * 100
x = training[,-1]
y = training$Accident_Severity

################################################## NB model
modelNB <- naiveBayes(as.factor(Accident_Severity) ~.,training)
modelNB
PredictNB <- predict(modelNB,newdata = testing )
PredictNB

str(PredictNB)
str(testing$Accident_Severity)
table(testing$Accident_Severity)
table(PredictNB)

confusionMatrix(PredictNB, as.factor(testing$Accident_Severity) )

#accuracy on trainig set NB
trNB<-training
tNB<-predict(modelNB,trNB,type="class")
mtab<-table(tNB,trNB$Accident_Severity)
confusionMatrix(mtab)


############################################### NN MODEL
modelNN<-nnet(as.factor(Accident_Severity)~Number_of_Vehicles
            +Day_of_Week+X1st_Road_Class+Road_Type+Speed_limit+

                          X2nd_Road_Class+Road_Surface_Conditions,data=training,size =8 ,decay = 0.2,maxit = 300)

summary(modelNN)
summary(modelNN$residuals)
PredictNN<-predict(modelNN,testing,type="class")
mtab<-table(PredictNN,testing$Accident_Severity)
confusionMatrix(mtab)
str(PredictNN)
str(testing$Accident_Severity)
table(testing$Accident_Severity)
table(PredictNN)

#accuracy on trainig set NN
trNN<-training
tNN<-predict(modelNN,trNN,type="class")
mtab<-table(tNN,trNN$Accident_Severity)
confusionMatrix(mtab)
#################################### random forest model
modelrf <- randomForest(
  as.factor(Accident_Severity) ~ .,
  data=training
)

Predictrf <- predict(modelrf,newdata = testing )
confusionMatrix(Predictrf, as.factor(testing$Accident_Severity) )

#accuracy on trainig set rf

trrf<-training
trf <- predict(modelrf,newdata = trrf )
confusionMatrix(trf, as.factor(trrf$Accident_Severity) )