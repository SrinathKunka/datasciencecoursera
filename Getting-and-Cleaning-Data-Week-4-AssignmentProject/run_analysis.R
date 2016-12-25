#Getting and Cleaning Data Course Project

library(dplyr)

# reading the train data
trainingData_X <- read.table("./CourseProject/UCI HAR Dataset/train/X_train.txt")
trainingData_Y <- read.table("./CourseProject/UCI HAR Dataset/train/Y_train.txt")
Sub_training <- read.table("./CourseProject/UCI HAR Dataset/train/subject_train.txt")

# reading test data
testData_X <- read.table("./CourseProject/UCI HAR Dataset/test/X_test.txt")
testData_Y <- read.table("./CourseProject/UCI HAR Dataset/test/Y_test.txt")
Sub_test <- read.table("./CourseProject/UCI HAR Dataset/test/subject_test.txt")

# reading data description
variable_names <- read.table("./CourseProject/UCI HAR Dataset/features.txt")

# reading activity labels
activity_labels <- read.table("./CourseProject/UCI HAR Dataset/activity_labels.txt")

# Merges the training and the test sets to create one data set.
total_X <- rbind(trainingData_X, testData_X)
total_Y <- rbind(trainingData_Y, testData_Y)
Sub_total <- rbind(Sub_training, Sub_test)

# Extracts only the measurements on the mean and standard deviation for each measurement.
selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
total_X <- total_X[,selected_var[,1]]

# Uses descriptive activity names to name the activities in the data set
colnames(total_Y) <- "activity"
total_Y$activitylabel <- factor(total_Y$activity, labels = as.character(activity_labels[,2]))
activitylabel <- total_Y[,-1]

# Appropriately labels the data set with descriptive variable names.
colnames(total_X) <- variable_names[selected_var[,1],2]

# From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(Sub_total) <- "subject"
total <- cbind(total_X, activitylabel, Sub_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "tidydata.txt", row.names = FALSE, col.names = TRUE)