rm(list=ls())
library("ggplot2")
library(magrittr) 
library(dplyr) 
library(lubridate)
library(sqldf)    
library(viridis)
setwd("D:\\Big Data/2000-16-traffic-flow-england-scotland-wales")
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
  return(dfms)
}

#Preprocessing
dfm1 <- preprocess("accidents_2005_to_2007.csv")
dfm2 <- preprocess("accidents_2009_to_2011.csv")
dfm3 <- preprocess("accidents_2012_to_2014.csv")

#Merging
dfm<-rbind(dfm1,dfm2)
dfm<- rbind(dfm,dfm3)

#Accidents per year
tableyear <- table(dfm$Year)
dfmyear <- data.frame(tableyear)
colnames(dfmyear)[1] <- "Year"
colnames(dfmyear)[2] <- "Number_of_accidents"
ggplot(data=dfmyear, aes(x=Year, y=Number_of_accidents, fill=Year)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1")

#Accidents per district
tabledistricts <- table(dfm$Local_Authority_.District.)
dfmdistricts <- data.frame(tabledistricts)
colnames(dfmdistricts)[1] <- "District"
colnames(dfmdistricts)[2] <- "Number_of_accidents"
#dfmdistricttabs <- dfmdistricts[order(dfmdistricts$Number_of_accidents),]
dfmdistricts <- dfmdistricts %>% 
  arrange(desc(Number_of_accidents)) 
dfmdistricts <- dfmdistricts[1:20,]
ggplot(dfmdistricts, aes(x=reorder(District,-Number_of_accidents), y=Number_of_accidents, fill=District)) +
  geom_bar(stat="identity") +
  labs(x="District")

#Road type vs number of accidents
tableroadtype <- table(dfm$Road_Type)
dfmroadtypetable <- data.frame(tableroadtype)
colnames(dfmroadtypetable)[1] <- "Road_type"
dfmroadtypetable <- dfmroadtypetable %>% 
  arrange(desc(Road_type)) %>%
  mutate(prop = Freq / sum(Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
ggplot(dfmroadtypetable, aes(x="", y=Freq, fill=Road_type)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")

#Number of accidents vs speed limit
tablespeed <- table(dfm$Speed_limit)
dfmspeed <- data.frame(tablespeed)
colnames(dfmspeed)[1] <- "Speed_limit"
colnames(dfmspeed)[2] <- "Number_of_accidents"
ggplot(dfmspeed, aes(x=reorder(Speed_limit,-Number_of_accidents), y=Number_of_accidents, fill=Speed_limit)) +
  geom_bar(stat="identity") +
  labs(x="District")+
  scale_fill_brewer(palette="Set2")

#Weekday / Weekend
tableday <- table(dfm$Day_of_Week)
dfmday <- data.frame(tableday)
weekend= (dfmday[1,2] + dfmday[7,2])/2
weekday= (dfmday[2,2] + dfmday[3,2]+dfmday[4,2] + dfmday[5,2]+dfmday[6,2])/5
dfmday <- data.frame(
  group = c("Average of weekend", "Average of weekday"),
  value = c(weekend,weekday)
)
ggplot(dfmday, aes(x=reorder(group,-value), y=value, fill=group)) +
  geom_bar(stat="identity") +
  labs(x="",y="Number of accidents")

#Accidents per highway
tablehighway <- table(dfm$Local_Authority_.Highway.)
dfmhighway <- data.frame(tablehighway)
colnames(dfmhighway)[1] <- "Local_Authority_highway"
colnames(dfmhighway)[2] <- "Number_of_accidents"
dfmhighway <- dfmhighway %>% 
  arrange(desc(Number_of_accidents)) 
dfmhighway <- dfmhighway[1:20,]
ggplot(dfmhighway, aes(x=reorder(Local_Authority_highway,-Number_of_accidents), y=Number_of_accidents, fill=Local_Authority_highway)) +
  geom_bar(stat="identity") +
  labs(x="Local_Authority_highway")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Junctions vs no junctions
tablejunction <- table(dfm$X2nd_Road_Class)
dfmjunction <- data.frame(tablejunction)
nojunction= (dfmjunction[1,2])
junction= dfmjunction[2,2] + dfmjunction[3,2]+dfmjunction[4,2] + dfmjunction[5,2]+dfmjunction[6,2]+dfmjunction[7,2]
dfmjunction <- data.frame(
  group = c("Accidents_at_junctions", "Accidents_not_at_junctions"),
  value = c(junction,nojunction)
)
ggplot(dfmjunction, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")

#junction control
tablejunccontrol <- table(dfm$Junction_Control)
dfmjunccontrol <- data.frame(tablejunccontrol)
dfmjunccontrol <- dfmjunccontrol[2:5,]
colnames(dfmjunccontrol)[1] <- "Junction_control"
dfmjunccontrol <- dfmjunccontrol %>% 
  arrange(desc(Freq)) 
ggplot(dfmjunccontrol, aes(x=reorder(Junction_control,-Freq), y=Freq, fill=Junction_control)) +
  geom_bar(stat="identity") +
  labs(x="Junction_control")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#time morning/afternoon/evening/night
hms <- hms(as.character(dfm$Time))
hours <- hour(hms)
dfm$hours <- hours
dfm$partofday <- ""
dfm$partofday [dfm$hours <= 12 & dfm$hours >=6] <- "Morning"
dfm$partofday [dfm$hours <= 17 & dfm$hours >12] <- "Afternoon"
dfm$partofday [dfm$hours <= 20 & dfm$hours >17] <- "Evening"
dfm$partofday [dfm$hours <= 24 & dfm$hours >20] <- "Night"
dfm$partofday [dfm$hours < 6 & dfm$hours >=0] <- "Night"
tabletimeofday <- table(dfm$partofday)
dfmtimeofday <- data.frame(tabletimeofday)
colnames(dfmtimeofday)[1] <- "Time_of_day"
dfmtimeofday <- dfmtimeofday %>% 
  arrange(desc(Freq)) 
ggplot(dfmtimeofday, aes(x=reorder(Time_of_day,-Freq), y=Freq, fill=Time_of_day)) +
  geom_bar(stat="identity") +
  labs(x="Time_of_day")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#season summer/winter/spring/autumn
months <- format(dfm$Date,"%m")
dfm$month <- months
dfm$season <- ""
dfm$season [dfm$month =="03" | dfm$month =="04" | dfm$month =="05" ] <- "Spring"
dfm$season [dfm$month=="06" | dfm$month =="07" | dfm$month =="08" ] <- "Summer"
dfm$season [dfm$month =="09" | dfm$month =="10" |dfm$month =="11"] <- "Autumn"
dfm$season [dfm$month =="12" | dfm$month =="01" |dfm$month =="02"] <- "Winter"
tableseason <- table(dfm$season)
dfmseason <- data.frame(tableseason)
ggplot(dfmseason, aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1)) +
  geom_bar(stat="identity") +
  labs(x="Season", y="Number of accidents")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#Road class
tableroadclass <- table(dfm$X1st_Road_Class)
dfmroadclass <- data.frame(tableroadclass)
ggplot(dfmroadclass, aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1)) +
  geom_bar(stat="identity") +
  labs(x="Road_class")


#severity , number of vehicles, number of accidents
tablesevvehicles <- table(dfm$Number_of_Vehicles,dfm$Accident_Severity)
dfmsevvehicles <- data.frame(tablesevvehicles)
dfmsevvehicles <- dfmsevvehicles[-c(6:27,33:54,60:81),]
colnames(dfmsevvehicles)[1] <- "Number_of_vehicles"
colnames(dfmsevvehicles)[2] <- "Severity"
ggplot(dfmsevvehicles, aes(fill=Severity, y=Number_of_vehicles, x=Freq)) + 
  geom_bar(position="stack", stat="identity") +
  labs(x="Number of accidents", y="Number of vehicles", "Severity")+
  coord_flip()


#Most freq long and lat
dfmlonglat <- sqldf('SELECT Longitude, Latitude , COUNT(*)
          FROM dfm 
          GROUP BY (Longitude||Latitude) 
          ORDER BY COUNT(*) DESC 
          LIMIT 10')
dfmlonglat$longlat <- paste(dfmlonglat$Longitude,",",dfmlonglat$Latitude)
dfmlonglat[c("Longitude","Latitude")] <- list(NULL)
colnames(dfmlonglat)[1] <- "Number_of_accidents"
colnames(dfmlonglat)[2] <- "Longitude_Latitude"
ggplot(dfmlonglat, aes(x=reorder(Longitude_Latitude,-Number_of_accidents), y=Number_of_accidents, fill=Longitude_Latitude)) +
  geom_bar(stat="identity") +
  labs(x="Longitude,Latitude")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#did police man attend accident scene over years
tablepolice <- table(dfm$Did_Police_Officer_Attend_Scene_of_Accident,dfm$Year)
dfmpolice <- data.frame(tablepolice)
dfmpolice <- dfmpolice[-c(1,4,7,10,13,16,19,22,25),]
colnames(dfmpolice)[1] <- "Did_police_officer_attend"
colnames(dfmpolice)[2] <- "Year"
ggplot(dfmpolice, aes(fill=Did_police_officer_attend, y=Freq, x=Year)) + 
  geom_bar(position="dodge", stat="identity")

#top 5 districts over years
dfmtopdistricts <- dfm[dfm$Local_Authority_.District. == 300 | dfm$Local_Authority_.District. == 204 | dfm$Local_Authority_.District. == 102 | dfm$Local_Authority_.District. == 91 | dfm$Local_Authority_.District. == 1,]
tabledistrictyear <- table(dfmtopdistricts$Local_Authority_.District.,dfmtopdistricts$Year)
dfmdistrictyear <- data.frame(tabledistrictyear)
colnames(dfmdistrictyear)[1] <- "District"
colnames(dfmdistrictyear)[2] <- "Year"
colnames(dfmdistrictyear)[3] <- "Number_of_accidents"
ggplot( dfmdistrictyear, aes(x=Year, y=Number_of_accidents, group=District, colour=District)) +
geom_line() +
geom_point()

#remove urban 3
tableurban <- table(dfm$Urban_or_Rural_Area)
dfmurban <- data.frame(tableurban)
ggplot(dfmurban, aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1)) +
  geom_bar(stat="identity") +
  labs(x="Urban or rural area")

#urban vs rural
dfmurbanrural <- subset(dfm, dfm$Urban_or_Rural_Area!=3) 
tableurbanrural <- table(dfmurbanrural$Urban_or_Rural_Area)
dfmurbanrural <- data.frame(tableurbanrural)
colnames(dfmurbanrural)[1] <- "Urban1_Rural2"
ggplot(dfmurbanrural, aes(x="", y=Freq, fill=Urban1_Rural2)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")


#Number of casualties per year
tablecasualtiesyear <- aggregate(cbind(dfm$Number_of_Casualties) ~ dfm$Year, data=dfm, FUN=sum)
dfmcasualtiesyear <- data.frame(tablecasualtiesyear)
colnames(dfmcasualtiesyear)[1] <- "Year"
colnames(dfmcasualtiesyear)[2] <- "Number_of_casualties"
ggplot(data=dfmcasualtiesyear, aes(x=factor(Year), y=Number_of_casualties, fill=factor(Year))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1")

#Number of casualties per district
tablecasualtiesdistricts <- aggregate(cbind(dfm$Number_of_Casualties) ~ dfm$Local_Authority_.District., data=dfm, FUN=sum)
dfmcasualtiesdistricts <- data.frame(tablecasualtiesdistricts)
colnames(dfmcasualtiesdistricts)[1] <- "District"
colnames(dfmcasualtiesdistricts)[2] <- "Number_of_casualties"
dfmcasualtiesdistricts <- dfmcasualtiesdistricts %>% 
  arrange(desc(Number_of_casualties)) 
dfmcasualtiesdistricts <- dfmcasualtiesdistricts[1:5,]
ggplot(data=dfmcasualtiesdistricts, aes(x=reorder(factor(District),-Number_of_casualties), y=Number_of_casualties, fill=factor(District))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1") +
  labs(x="Districts")
