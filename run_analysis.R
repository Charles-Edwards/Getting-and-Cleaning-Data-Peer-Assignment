library(dplyr)

######---1---Merges the training and the test sets to create one data set###########

#zip file link
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
File <- "UCI HAR Dataset.zip"

#download data if doesn't exist
if (!file.exists(File )) {
  download.file(Url , File , mode = "wb")
}

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(File )
}

###read data into R
training_Subjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
training_Values <- read.table(file.path(dataPath, "train", "X_train.txt"))
training_Activity <- read.table(file.path(dataPath, "train", "y_train.txt"))
test_Subjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
test_Values <- read.table(file.path(dataPath, "test", "X_test.txt"))
test_Activity <- read.table(file.path(dataPath, "test", "y_test.txt"))

Features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

###Combine data into single table
Activity_Data <- rbind(
  cbind(training_Subjects, training_Values, training_Activity ),
  cbind(test_Subjects, test_Values, test_Activity)
)

#rename columns appropriately
colnames(Activity_Data) <- c("subject", Features[, 2], "activity")

######---1---Merges the training and the test sets to create one data set###########

######---2---Extracts only the measurements on the mean and standard deviation for each measurement.###########

Activity_Data[ , grepl( "mean" , names( Activity_Data) ) ] -> meandf
Activity_Data[ , grepl( "std" , names( Activity_Data) ) ] -> stddf
Activity_Data[ , grepl( "activity" , names( Activity_Data) ) ] -> activity
Activity_Data[ , grepl( "subject" , names( Activity_Data) ) ] -> subject
cbind(subject, activity, meandf,stddf) -> stdmeandf


######---2---Extracts only the measurements on the mean and standard deviation for each measurement.###########

######---3---Uses descriptive activity names to name the activities in the data set.###########

stdmeandf$activity <- factor(stdmeandf$activity, levels = activities[, 1], labels = activities[, 2])

######---3---Uses descriptive activity names to name the activities in the data set.###########

######---4---Appropriately labels the data set with descriptive variable names.###########

gsub("[[:punct:]]", "",colnames(stdmeandf)) -> colnames(stdmeandf)

gsub("tBody", "Time_Body_",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("fBody", "Fast_Fourier_Body_",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("tGravity", "Time_Gravity_",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("Mag", "Magnitude_",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("Acc", "Acceleration_",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("Jerk", "Jerk_",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("Gyro", "Gyro_",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("BodyA", "Body_A",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("BodyG", "Body_G",colnames(stdmeandf)) -> colnames(stdmeandf)
gsub("Body_Body_", "Body_",colnames(stdmeandf)) -> colnames(stdmeandf)

######---4---Appropriately labels the data set with descriptive variable names.###########

######---5---From the data set in step 4, creates a second, independent tidy 
#data set with the average of each variable for each activity and each subject.###########


#average on grouped unique subject and activity combinations
stdmeandf_avg <- stdmeandf%>% 
  group_by(stdmeandf$subject, stdmeandf$activity) %>%
  summarise_each(funs(mean))

##remove excess columns
stdmeandf_avg[3:4] <- NULL

###rename columns appropriately
colnames(stdmeandf_avg) <- c("Subject","Activity",colnames(stdmeandf_avg[3:ncol(stdmeandf_avg)]))

####order ascending by subject number 
stdmeandf_avg <- stdmeandf_avg[order(stdmeandf_avg$Subject),]

###output to txt file 
write.table(stdmeandf_avg, "std_mean_data_clean.txt", row.name=FALSE )
