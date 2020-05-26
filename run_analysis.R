#Storing file URL
FileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#destination zip file
DestFile <- paste0(getwd(), "/", "dataweek4.zip")

#download zip file
download.file(FileUrl, DestFile)

#unzip file
unzip("dataweek4.zip")
FilePath <- paste0(getwd(), "/UCI HAR Dataset")

##### Q 1 Merges the training and the test sets to create one data set

#reading required files & preparing for merge
library(data.table)
library(dplyr)

#storing activity labels and features
ActivityLabels <-read.table(paste0(FilePath,"/activity_labels.txt"))[,2] #Extracting second column
Features <-read.table(paste0(FilePath,"/features.txt"))[,2]

#Preparing Test File
TestSubjects <-read.table(paste0(FilePath,"/test/subject_test.txt"))
names(TestSubjects) <- "Subjects"

TestData <-read.table(paste0(FilePath,"/test/X_test.txt"))
names(TestData) <- Features

TestActivityData <-read.table(paste0(FilePath,"/test/y_test.txt"))
Activity <- factor(TestActivityData$V1, labels = ActivityLabels) #Buiding Activity coulmn as a factor for Test


DfTest <- cbind(TestSubjects, TestData, Activity) #Test Data frame ready to merge

#Prearing Train File

TrainSubjects <-read.table(paste0(FilePath,"/train/subject_train.txt"))
names(TrainSubjects) <- "Subjects"

TrainData <-read.table(paste0(FilePath,"/train/X_train.txt"))
names(TrainData) <- Features

TrainActivityData <-read.table(paste0(FilePath,"/train/y_train.txt"))
Activity <- factor(TrainActivityData$V1, labels = ActivityLabels) #Buiding Activity coulmn as a factor for Train Dataset


DfTrain <- cbind(TrainSubjects, TrainData, Activity) #Train Data frame ready to merge

DfBoth <- rbind(DfTrain, DfTest) #Merging both the data frames row-wise

# Q 2 Extracts only the measurements on the mean and standard deviation for each measurement.

 # indices <- grep("mean|std", names(DfBoth)) # Indices will have column numbers of Columns containing string "means or std in their names
indices <- grep ("mean\\(\\)|std\\(\\)", names(DfBoth)) #Indices will have column numbers of Columns containing string "means or std in their names


# Subsetting Data to contain only columns subjects(column 1), Activity (last Column) and  intermediate Columns matching criteria
DfBoth <- DfBoth[,c(1, indices, ncol(DfBoth))] 

# Q 3 Uses descriptive activity names to name the activities in the data set

# Please note that I have already built both the tables train and test with Activity columns as factors in Question 1

DfBoth$Activity <- factor(DfBoth$Activity, labels=ActivityLabels) #Converting Activity column to factor.

# Q 4 Appropriately labels the data set with descriptive variable names.

names(DfBoth)<-gsub("tBodyAcc-","Body acceleration signal in time domain (from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("tBodyAccMag-","Body acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("tBodyAccJerk-","Body acceleration jerk signal in time domain (from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("tBodyAccJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform (from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("tGravityAcc-","Gravity acceleration signal in time domain (from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("tGravityAccMag-","Gravity acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("tBodyGyro-","Body acceleration signal in time domain (from the gyroscope)",names(DfBoth))
names(DfBoth)<-gsub("tBodyGyroMag-","Body acceleration signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(DfBoth))
names(DfBoth)<-gsub("tBodyGyroJerk-","Body acceleration jerk signal in time domain (from the gyroscope)",names(DfBoth))
names(DfBoth)<-gsub("tBodyGyroJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(DfBoth))
names(DfBoth)<-gsub("fBodyAcc-","Body acceleration signal in frequence domain (from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("fBodyAccMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform(from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("fBodyAccJerk-","Body acceleration jerk signal in frequence domain (from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("fBodyGyro-","Body acceleration signal in frequence domain (from the gyroscope)",names(DfBoth))
names(DfBoth)<-gsub("fBodyAccJerkMag-","Body acceleration jerk signal in frequence domain applied to Fast Fourrier Transform (from the accelerometer)",names(DfBoth))
names(DfBoth)<-gsub("fBodyGyroMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform (from the gyroscope)",names(DfBoth))
names(DfBoth)<-gsub("mean()", "MEAN", names(DfBoth))
names(DfBoth)<-gsub("std()", "SD", names(DfBoth))


#question 5 :tidy data set with the average of each variable for each activity and each subject
tidydata<-DfBoth %>% group_by(Subjects, Activity) %>% summarise_all(mean)
write.table(tidydata, "TidyData.txt", row.names = FALSE)


