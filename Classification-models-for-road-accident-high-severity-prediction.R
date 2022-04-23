# Importing libraries
library(dplyr)
library(lubridate)
library(ggpubr)
library(cowplot)
library(praznik)
library(caret)
library(randomForest)
library(tidyverse)
library(kernlab)
library(e1071)
library(dummies)
library(class)
library(rpart)
library(rpart.plot)
require(caTools)
library(ipred)
library(DMwR)
library(vcd)
library(gmodels)
library(descr)
library(lsr)
library(DataExplorer)
library(mlbench)
library(visreg) 
library(margins)
library(rcompanion)
library(ROCR)       
library(FSelector) 
library(corrplot)
library(rpart)


# Import raw dataset 
accidents_2020 <- read.csv("Road-Safety-Data-Accidents-2020.csv")


# EXPLORATORY ANALYSIS
# dimensiones of data sets
dim(accidents_2020)
# dataset structure
str(accidents_2020)
# dataset structure - plot
DataExplorer::plot_str(accidents_2020)
# summary
summary(accidents_2020)
# metrics about variables
DataExplorer::plot_intro(accidents_2020)


# reducing the number of variables, as some variables are not relevant
accidents <- accidents_2020[-c(2,3,4,5,6,7,8,10,11,15,16,17,19,24,25,34,35,36)]
# checking structure of new dataset
str(accidents)
DataExplorer::plot_str(accidents)

# check missing data only from selected variables
DataExplorer::plot_intro(accidents)
accidents[!complete.cases(accidents), ]

# INITIAL DATA TRANSFORMATION

# ACCIDENT SEVERITY

# check how many records are from each of the categories of the targeted variable "accident severity"
accidents %>% 
  group_by(Accident_Severity)%>%
  summarise(count = n())

# the data set is clearly unbalanced
# changing severity from 1, 2 and 3 to 1 and 2, in order to have only two classes
# this way the number of records in each of the classes is more balanced
accidents$Accident_Severity[accidents$Accident_Severity == 1] <- 1
accidents$Accident_Severity[accidents$Accident_Severity == 2] <- 1
accidents$Accident_Severity[accidents$Accident_Severity == 3] <- 0

# check again
accidents %>% group_by(Accident_Severity)%>%
  summarize(count = n()) #the classes are still unbalanced

# changing to factor
accidents$Accident_Severity <- factor(accidents$Accident_Severity, levels = c(0,1))

# renaming the variable
accidents <- accidents %>% dplyr::rename("accidentseverity" = "Accident_Severity")


# TIME OF THE DAY

# create new variable hour from the existing Time variable
accidents$hour <- as.numeric(gsub("\\:.*$", "", accidents$Time))

# change the time into morning, afternoon, evening and noon
accidents <- accidents %>%
  add_column(timeday = ifelse (accidents$hour >= 6 & accidents$hour <= 12,"Morning",
                               ifelse (accidents$hour >12 & accidents$hour <=17,"Afternoon",
                                       ifelse (accidents$hour >18 & accidents$hour <24,"Evening", "Noon"))))
# change class and add levels
accidents$timeday <- factor(accidents$timeday, levels = c("Morning", "Afternoon", "Evening", "Noon"))

# count number of n/a
sum(is.na(accidents$timeday)) #64 na
# remove na, which are only 64 values
accidents <- subset(accidents,!is.na(accidents$timeday))


# MONTH

# transform date as month
accidents$Date <- as.Date(accidents$Date, "%d/%m/%Y")
accidents$month <- months(accidents$Date)

# change class and add levels
accidents$month <- factor(accidents$month, levels = c("January", "February","March","April","May","June","July",
                                                      "August","September","October","November","December"))
# check if there are any nas
sum(is.na(accidents$month))# there is no nas

# FIRST ROAD CLASS

# change 1st road class
accidents <- accidents %>%
  add_column(road1class = ifelse (accidents$X1st_Road_Class == 1 | accidents$X1st_Road_Class == 2,"B",
                                  ifelse (accidents$X1st_Road_Class == 3 |  accidents$X1st_Road_Class == "4","M",
                                          ifelse (accidents$X1st_Road_Class == 5 ,"S","U"))))

# change to factor and add levels
accidents$road1class <- factor(accidents$road1class, levels = c("B","M","S","U"))
#check to see if there are any nas
sum(is.na(accidents$road1class))

# DAY OF THE WEEK

# change to factor and add levels
accidents$weekday <- factor(accidents$Day_of_Week, levels = c(1,2,3,4,5,6,7))
# check to see if there are any nas
sum(is.na(accidents$weekday))


# ROAD TYPE
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(roadtype = ifelse (accidents$Road_Type == 1, "roundabout",
                                ifelse(accidents$Road_Type == 2, "onewaystreet",
                                       ifelse(accidents$Road_Type == 3,"dualcarriage",
                                              ifelse(accidents$Road_Type == 6, "singlecarriage",     
                                                     ifelse (accidents$Road_Type == 9 |  accidents$Road_Type == -1,"unknown","sliproad"))))))
# change class to factors                               
accidents$roadtype <- factor(accidents$roadtype)
# check to see if there are any nas
sum(is.na(accidents$weekday))


# SPEED LIMIT
# change to factor and add the levels
accidents$speedlimit <- factor(accidents$Speed_limit, levels = c(20,30,40,50,60,70,-1))



# JUNCTION DETAIL
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(junctiondetail = ifelse (accidents$Junction_Detail == 0, "notjunction",
                                      ifelse(accidents$Junction_Detail == 1 ,"roundabout",
                                             ifelse(accidents$Junction_Detail == 2,"miniroundabout",
                                                    ifelse (accidents$Junction_Detail == 3,"torstaggeredjunction",
                                                            ifelse (accidents$Junction_Detail == 5,"sliproad",
                                                                    ifelse (accidents$Junction_Detail == 6,"crossroads",
                                                                            ifelse (accidents$Junction_Detail == 7, "morethan4arms_notroundabout",
                                                                                    ifelse(accidents$Junction_Detail == 8,"privatedrive",
                                                                                           ifelse(accidents$Junction_Detail == 9,"otherjunction","unknown"))))))))))
# changed to factor
accidents$junctiondetail <- factor(accidents$junctiondetail)

# JUNCTION CONTROL
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(junctioncontrol = ifelse (accidents$Junction_Control == 0, "notjunction",
                                       ifelse(accidents$Junction_Control == 1 ,"authorisedperson",
                                              ifelse(accidents$Junction_Control == 2,"autotrafficsignal",
                                                     ifelse (accidents$Junction_Control == 3,"stopsign",
                                                             ifelse (accidents$Junction_Control == 4,"giveawayoruncontrolled","unknown"))))))
# changed to factor
accidents$junctioncontrol <- factor(accidents$junctioncontrol)


# PEDESTRIAN CROSSING HUMAN CONTROL
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(pedestrian = ifelse (accidents$Pedestrian_Crossing.Human_Control == 0, "none50m",
                                  ifelse(accidents$Pedestrian_Crossing.Human_Control == 1 ,"schoolpatrol",
                                         ifelse(accidents$Pedestrian_Crossing.Human_Control == 2,"authorisedperson","unknown"))))
# changed to factor
accidents$pedestrian <- factor(accidents$pedestrian)


# PEDRESTIAN CROSSING PHYSICAL FACILITIES
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(crossing = ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 0, "notcrossing50m",
                                ifelse(accidents$Pedestrian_Crossing.Physical_Facilities == 1 ,"zebra",
                                       ifelse(accidents$Pedestrian_Crossing.Physical_Facilities == 4,"lightcrossing",
                                              ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 5,"trafficjunction",
                                                      ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 7,"subway",
                                                              ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 8,"centralrefuge","unknown")))))))
# changed to factor
accidents$crossing <- factor(accidents$crossing)



# LIGHT CONDITIONS
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(light = ifelse (accidents$Light_Conditions == 1, "day",
                             ifelse(accidents$Light_Conditions == 4 ,"darklightlit",
                                    ifelse(accidents$Light_Conditions == 5,"darklightunits",
                                           ifelse (accidents$Light_Conditions == 6,"darknolight",
                                                   ifelse (accidents$Light_Conditions == 7,"darklightunknown","unknown"))))))
# changed to factor
accidents$light <- factor(accidents$light)



# WEATHER CONDITIONS
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(weather = ifelse (accidents$Weather_Conditions == 1, "ok",
                               ifelse (accidents$Weather_Conditions == 2 ,"rain",
                                       ifelse (accidents$Weather_Conditions == 3 ,"snow",
                                               ifelse (accidents$Weather_Conditions == 4 ,"wind",
                                                       ifelse (accidents$Weather_Conditions == 5 ,"rainwind",
                                                               ifelse (accidents$Weather_Conditions == 6 ,"snowwind",
                                                                       ifelse (accidents$Weather_Conditions == 7 ,"frostmist",
                                                                               ifelse (accidents$Weather_Conditions == 8 ,"other","unknown")))))))))

# changed to factor
accidents$weather <- factor(accidents$weather)


# ROADSURFACE CONDITIONS
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(road = ifelse (accidents$Road_Surface_Conditions == 1, "dry",
                            ifelse (accidents$Road_Surface_Conditions == 2 ,"wet",
                                    ifelse (accidents$Road_Surface_Conditions == 3 ,"snow",
                                            ifelse (accidents$Road_Surface_Conditions == 4 ,"ice",
                                                    ifelse (accidents$Road_Surface_Conditions == 5 ,"flood",
                                                            ifelse (accidents$Road_Surface_Conditions == 6 ,"oil",
                                                                    ifelse (accidents$Road_Surface_Conditions == 7 ,"muld","unknown"))))))))

# changed to factor
accidents$road <- factor(accidents$road)



# SPECIAL CONDITIONS AT SITE
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(special = ifelse (accidents$Special_Conditions_at_Site == 0, "none",
                               ifelse (accidents$Special_Conditions_at_Site == 1,"signalout",
                                       ifelse (accidents$Special_Conditions_at_Site == 2 | accidents$Special_Conditions_at_Site == 3,"defectivesignal",
                                               ifelse (accidents$Special_Conditions_at_Site == 4 ,"roadworks",
                                                       ifelse (accidents$Special_Conditions_at_Site == 5 ,"defectiveroad",
                                                               ifelse (accidents$Special_Conditions_at_Site == 6 ,"oil",
                                                                       ifelse (accidents$Special_Conditions_at_Site == 7 ,"muld","unknown"))))))))

# changed to factor
accidents$special <- factor(accidents$special)




# CARRIAGE HAZARDS
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(carriagehazards = ifelse (accidents$Carriageway_Hazards == 0, "none",
                                       ifelse (accidents$Carriageway_Hazards == 1,"vehicleloan",
                                               ifelse (accidents$Carriageway_Hazards == 2 ,"object",
                                                       ifelse (accidents$Carriageway_Hazards == 3 ,"previousaccident",
                                                               ifelse (accidents$Carriageway_Hazards == 4 ,"dog",
                                                                       ifelse (accidents$Carriageway_Hazards == 5 | accidents$Carriageway_Hazards == 7 ,"otheranimal",
                                                                               ifelse (accidents$Carriageway_Hazards == 6 ,"pedestrian","unknown"))))))))

# changed to factor
accidents$carriagehazards <- factor(accidents$carriagehazards)




# URBAN OR RUAL AREA
# reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(urbanrural = ifelse (accidents$Urban_or_Rural_Area == 1, "urban",
                                  ifelse (accidents$Urban_or_Rural_Area == 2,"rural","unknown")))

# changed to factor
accidents$urbanrural <- factor(accidents$urbanrural)


# DROP THE OLD VARIABLES
# drop date, time, hour
accidents <- accidents[-c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
# doblecheck the variables remaining are the needed ones
dim(accidents)
str(accidents)
summary(accidents)



# EXPLORATORY ANALYSIS

# grouping
dayweek <- accidents %>%
  group_by(accidentseverity, weekday)%>%
  summarize(n = n())
# creating plot
dayweekplot <- ggplot(dayweek, aes(x=weekday, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Day of the Week", x = "Day of the Week", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))


# grouping
time <- accidents %>%
  group_by(accidentseverity, timeday)%>%
  summarize(n = n())
# creating plot
timeplot <- ggplot(time, aes(x=timeday, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Time of the Day", x = "Time of the Day", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))


# grouping
roadclass <- accidents %>%
  group_by(accidentseverity, road1class)%>%
  summarize(n = n())
# creating plot
roadclassplot <- ggplot(roadclass, aes(x=road1class, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Class of the Road 1", x = "Road 1 Class", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
month <- accidents %>%
  group_by(accidentseverity, month)%>%
  summarize(n = n())
# creating plot
monthplot <- ggplot(month, aes(x=month, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Month", x = "Month", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
roadtype <- accidents %>%
  group_by(accidentseverity, roadtype)%>%
  summarize(n = n())
# creating plot
roadtypeplot <- ggplot(roadtype, aes(x=roadtype, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Road Type", x = "Road Type", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
speedlimit <- accidents %>%
  group_by(accidentseverity, speedlimit)%>%
  summarize(n = n())
# creating plot
speedlimitplot <- ggplot(speedlimit, aes(x=speedlimit, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Speed Limit (MpH)", x = "Speed Limit (MpH)", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
junctiondetail <- accidents %>%
  group_by(accidentseverity, junctiondetail)%>%
  summarize(n = n())
#creating plot
junctiondetailplot <- ggplot(junctiondetail, aes(x=junctiondetail, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Junction Detail", x = "Junction Detail", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))


#grouping
junctioncontrol <- accidents %>%
  group_by(accidentseverity, junctioncontrol)%>%
  summarize(n = n())
# creating plot
junctioncontrolplot <- ggplot(junctioncontrol, aes(x=junctioncontrol, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Junction Control", x = "Junction Control", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
pedestrian <- accidents %>%
  group_by(accidentseverity, pedestrian)%>%
  summarize(n = n())
# creating plot
pedestrianplot <- ggplot(pedestrian, aes(x=pedestrian, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Pedestrian Crossing Human Control", x = "Pedestrian Crossing Human Control", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
crossing <- accidents %>%
  group_by(accidentseverity, crossing)%>%
  summarize(n = n())
# creating plot
crossingplot <- ggplot(crossing, aes(x=crossing, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Pedestrian Crossing Physical Facilities", x = "Pedestrian Crossing Physical Facilities", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
light <- accidents %>%
  group_by(accidentseverity, light)%>%
  summarize(n = n())
# creating plot
lightplot <- ggplot(light, aes(x=light, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Light Conditions", x = "Light Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
weather <- accidents %>%
  group_by(accidentseverity, weather)%>%
  summarize(n = n())
# creating plot
weatherplot <- ggplot(weather, aes(x=weather, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Weather Conditions", x = "Weather Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
road <- accidents %>%
  group_by(accidentseverity, road)%>%
  summarize(n = n())
# creating plot
roadplot <- ggplot(road, aes(x=road, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Road Surface Conditions", x = "Road Surface Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
special <- accidents %>%
  group_by(accidentseverity, special)%>%
  summarize(n = n())
# creating plot
specialplot <- ggplot(special, aes(x=special, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Special Conditions at Site", x = "Special Conditions at Site", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
urbanrural <- accidents %>%
  group_by(accidentseverity, urbanrural)%>%
  summarize(n = n())
# creating plot
urbanruralplot <- ggplot(urbanrural, aes(x=urbanrural, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Urban - Rural", x = "Urban - Rural", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

# grouping
carriagehazards <- accidents %>%
  group_by(accidentseverity, carriagehazards)%>%
  summarize(n = n())
# creating plot
carriagehazardsplot <- ggplot(carriagehazards, aes(x=carriagehazards, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Carriageway Hazards", x = "Carriageway Hazards", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



# plotting all the previous plotts
plot_grid(dayweekplot, timeplot, roadclassplot,
          monthplot,nrow = 2)

plot_grid(roadtypeplot,speedlimitplot,junctiondetailplot,
          junctioncontrolplot,nrow = 2)

plot_grid(pedestrianplot,crossingplot, roadplot,
          specialplot,nrow = 2)

plot_grid(urbanruralplot, nrow = 2, ncol =2)

