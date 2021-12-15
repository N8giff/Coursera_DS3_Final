#PEER GRADED ASSIGNMENT#

#Criteria...
    #1. Merges the training and the test sets to create one data set. 

    #2. Extracts only the measurements on the mean and standard deviation for each measurement. 

    #3. Uses descriptive activity names to name the activities in the data set

    #4. Appropriately labels the data set with descriptive variable names. 

    #5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#clear objects
rm(list=ls())

#set working directory
old.dir <- getwd()
setwd('~/Desktop/R Projects/Coursera/Getting and Cleaning Data/Week 4/Peer Assignment')

#load libraries
library(tidyverse)

#Download zip file and unzip

zipfile <- 'Coursera_DS3_Final.zip' 
filename <- 'UCI HAR Dataset'

if(!file.exists(zipfile)) {
  URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(URL, destfile= './Coursera_DS3_Final.zip', method='curl')
}

if(!file.exists(filename)) {
  unzip(zipfile)
}


#1. Merge training and the test sets to create one data set

MergedData <- './UCI HAR Dataset/df_merged.csv'

if(file.exists(MergedData)) {
  
  df_merged <- read.csv(MergedData)
  
} else {

    features <- read.table("UCI HAR Dataset/features.txt", 
                            col.names = c("n","fx"))
    activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                            col.names = c("code", "activity"))

    subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                            col.names = "subject")
    x_test <- read.table("UCI HAR Dataset/test/X_test.txt", 
                            col.names = features$fx)
    y_test <- read.table("UCI HAR Dataset/test/y_test.txt", 
                            col.names = "code")

    subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                            col.names = "subject")
    x_train <- read.table("UCI HAR Dataset/train/X_train.txt", 
                            col.names = features$fx)
    y_train <- read.table("UCI HAR Dataset/train/y_train.txt", 
                            col.names = "code")

    #Combine Subject, test, and train, then merge all

    x <- rbind(x_test, x_train)
    y <- rbind(y_test, y_train)
    subject <- rbind(subject_train, subject_test)

    df_merged <- cbind(x,y,subject)

    #Export df_merged
    write.csv(df_merged, './df_merged.csv')

}


#2. Extract only the measurements on the mean and standard deviation for each measurement.

df_trim <- df_merged %>% 
            select(subject, 
                   code, 
                   contains("mean"), 
                   contains("std"))

#3. Use descriptive activity names to name the activities in the data set

df_trim$code <- activities[df_trim$code, 2]


#4. Appropriately label the data set with descriptive variable names

#column name changes
names(df_trim)[1] = "Subject"
names(df_trim)[2] = "Activity"

names(df_trim) <- gsub("Acc", "Accelerometer", names(df_trim))
names(df_trim) <- gsub("Gyro", "Gyroscope", names(df_trim))
names(df_trim) <- gsub("BodyBody", "Body", names(df_trim))

names(df_trim) <- gsub("Mag", "Magnitude", names(df_trim))
names(df_trim) <- gsub("angle", "Angle", names(df_trim))
names(df_trim) <- gsub("gravity", "Gravity", names(df_trim))

names(df_trim) <- gsub("^t", "Time", names(df_trim))
names(df_trim) <- gsub("^f", "Frequency", names(df_trim))

names(df_trim) <- gsub("-mean()", "Mean", names(df_trim), ignore.case = TRUE)
names(df_trim) <- gsub("-std()", "STD", names(df_trim), ignore.case = TRUE)
names(df_trim) <- gsub("-freq()", "Frequency", names(df_trim), ignore.case = TRUE)


#5. Creates a second tidy data set with the average of each variable for each activity and each subject.

TidyData <- df_trim %>%
  group_by(Subject, Activity) %>%
  summarize_all(funs(mean))

#Export!

write.table(TidyData, './TidyData.txt', row.name=FALSE)

write.csv(TidyData, './TidyData.csv')

