##########################################################################
## The purpose of this project is to demonstrate your ability
## to collect, work with, and clean a data set. 
## The goal is to prepare tidy data that can be used for later analysis
##########################################################################
 
## Preprocess: Downloading data files
#####################################
#fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="./run_analysis_data.zip")
#unzip("./run_analysis_data.zip",exdir = ".")
library(plyr)
library(dplyr)
library(data.table)

## 1.Merges the training and the test sets to create one data set. 
##################################################################
fileName = ".\\UCI HAR Dataset\\train\\X_train.txt"
train <- read.table(fileName, header=FALSE)
fileName = ".\\UCI HAR Dataset\\test\\X_test.txt"
test <- read.table(fileName, header=FALSE)
data <- rbind(traint,test)

## 2.Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################
fileName = ".\\UCI HAR Dataset\\features.txt"
features <- read.table(fileName, header=FALSE, stringsAsFactors = FALSE)
meanStdFeatures <- features[grep("mean|std", features[,2]),]
meanStdData <- data[,meanStdFeatures[,1]]

fileName = ".\\UCI HAR Dataset\\train\\Subject_train.txt"
subjectTrain <- read.table(fileName, header=FALSE)
fileName = "run_analysis\\UCI HAR Dataset\\test\\Subject_test.txt"
subjectTest <- read.table(fileName, header=FALSE)
subjectData <- rbind(subjectTrain,subjectTest)

fileName = ".\\UCI HAR Dataset\\train\\y_train.txt"
activityTrain <- read.table(fileName, header=FALSE)
fileName = ".\\UCI HAR Dataset\\test\\y_test.txt"
activityTest <- read.table(fileName, header=FALSE)
activityData <- rbind(activityTrain,activityTest)

meanStdData <- cbind(meanStdData, subjectData)
meanStdData <- cbind(meanStdData, activityData)
names(meanStdData) <- c(meanStdFeatures[,2], "Subject", "Activity")

## 3.Uses descriptive activity names to name the activities in the data set.
############################################################################
activitiesIds <- meanStdData[,"Activity"]
activitiesFactors <- as.factor(activitiesIds)
activitiesFactors = revalue(activitiesFactors, 
      c("1"="WALKING", "2"="WALKING_UPSTAIRS","3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
meanStdData[,"Activity"] = activitiesFactors

## 4.Appropriately labels the data set with descriptive variable names.
############################################################################
names(meanStdData) <- c(meanStdFeatures[,2], "Subject", "Activity")

## 5.From the data set in step 4, creates a second, 
## independent tidy data set with the average of each variable for each activity and each subject.
##################################################################################################
tidyDataTable <- data.table(meanStdData)
avgTidyDataTable <- tidyDataTable[, lapply(.SD,mean), by=c("Subject","Activity")]
newColNames = sapply(names(avgTidyDataTable)[-(1:2)], function(name) paste("mean(", name, ")", sep=""))
setnames(avgTidyDataTable, names(avgTidyDataTable), c("Subject", "Activity", newColNames))
avgTidyDataTable<-arrange(avgTidyDataTable,Subject,desc(Activity))
write.table(avgTidyDataTable, "run_analysis/MeasureAvgTidySet2.txt", sep="\t\t", row.names = FALSE)  

