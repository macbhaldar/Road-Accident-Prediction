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
