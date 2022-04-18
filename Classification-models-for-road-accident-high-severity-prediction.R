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

