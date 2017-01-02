# Course: Getting and Cleaning Data in R
#
# Project: run_analysis.R (this file)
#

#clean up workspace
#rm(list=ls())

#set directory with input files
setwd("~/datasciencecoursera/project")

#get the dataset
filename <- "dataset.zip"
if (!file.exists(filename)) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, filename, method="curl")
}

#unzip file
if(!file.exists("UCI HAR Dataset")) {
        unzip(filename)
}
#and go to the unzipped directory
setwd("./UCI HAR Dataset")

#get needed libraries, check, if they're installed or not
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
library(plyr); library(dplyr)


#read activity labels and features
activity_labels <- read.table("activity_labels.txt") #links labels with their activity names (6)
       features <- read.table("features.txt") #lists all features (561) - type of measurement
       
#give activity_labels table descriptive column names:
       colnames(activity_labels) <- c("activitylabel","activity")
       
#read test datasets
       testX <- read.table("test/X_test.txt") #test set (2947 measurements <rows> of 561 features <columns>)
       testY <- read.table("test/Y_test.txt") #test labels for each measurement(2947)
       test_subject <- read.table("test/subject_test.txt") # subject ID (range 1-30)

       #give the datasets descriptive column names
       colnames(test_subject) <- "subjectid"
       colnames(testX) <- features[,2]      
       colnames(testY) <- "activitylabel"
       
#merge test datasets (activity, subject, feature)
       test_data <- cbind(testY, test_subject, testX)
       
#read train datasets
       trainX <- read.table("train/X_train.txt") #training set (7352 measurements <rows> of 
                                                 #561 features <columns>)
       trainY <- read.table("train/Y_train.txt") #training labels for each measurement (7352)
       train_subject <- read.table("train/subject_train.txt") # subject ID (range 1-30)
       
       #give the datasets descriptive column names
       colnames(train_subject) = "subjectid"
       colnames(trainX) = features[,2]      
       colnames(trainY) = "activitylabel"
       
#merge train datasets (activity, subject, feature)
       train_data <- cbind(trainY, train_subject, trainX)
       
#merge train and test datasets
       data <- rbind(test_data, train_data)
       
#extract only the measurements on the mean and standard deviation for each measurement.
#together with the activity and subject ID
       data <- data[grep("activitylabel|subjectid|.mean.|.std.",names(data))]
       
#sort data by the activity label and subject id
       data <- arrange(data,activitylabel,subjectid)
       
#merge "activity_labels" and "data" to get descriptive names of the activities
       data <-merge(activity_labels,data,by="activitylabel")
       
#we don't need the label number anymore (having activity name), let's remove it from data
       data <- select(data,-activitylabel)
       
#cleanup measurement names     
       cleanNames <- names(data)
       
       #for each column name:
       for (i in 1:length(cleanNames)) {
               #remove "()"
               cleanNames[i] <- gsub("\\()","",cleanNames[i]) 
               #remove "-"
               cleanNames[i] <- gsub("\\-","",cleanNames[i])
               #"t" means "time"
               cleanNames[i] <- gsub("^t","time",cleanNames[i])
               #"f" means "frequency" (freq)
               cleanNames[i] <- gsub("^f","freq",cleanNames[i])
               #"acc" will be "accel" for acceleration
               cleanNames[i] <- gsub("[Aa]cc","accel",cleanNames[i])
               #"std" is "stddev" for standard deviation
               cleanNames[i] <- gsub("std","stddev",cleanNames[i])
               #remove doubled "BodyBody"
               cleanNames[i] <- gsub("[Bb]ody[Bb]ody","body",cleanNames[i])
               #"magnitude" instead of "mag"
               cleanNames[i] <- gsub("[Mm]ag","magnitude",cleanNames[i])
               #all lowercase
               cleanNames[i] <- tolower(cleanNames[i])
       }
       
#reassign new column names to the dataset
       colnames(data) <- cleanNames
       
#create a tidy data set based on "data"with the average of each variable for each 
#activity and each subject
              
       #staring from 3, because 2 first columns are activity and subject ID                
        tidy_data <- ddply(data, .(subjectid, activity), function(x) colMeans(x[, 3:ncol(x)])) 

       #export a .txt file with tidy data:
        write.table(tidy_data, './tidy_data.txt', row.names=FALSE, sep='\t')
              