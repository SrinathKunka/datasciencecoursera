---
title: "Readme.md"
output: html_document
---

Getting-and-Cleaning-Data-Week-4-Assignment-Project

The repo has been created to finish the assignment for week 4 of Getting and Cleaning Data Coursera course.

Below are the steps followed in run_analysis.R :

The below script lines will help to download and unzip the data

#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="./CourseProject/Dataset.zip")

#UnZipping the downloaded file
#unzip(zipfile="courseProject/Dataset.zip",exdir="./courseProject")


Data description

variables in data X : are sensor signals measured with waist-mounted smartphone from 30 subjects. 
variable in data Y : indicates activity type the subjects performed during recording.

Code explaination


The code combined training dataset and test dataset, and extracted partial variables to create another dataset with the averages of each variable for each activity.

New dataset

The new generated dataset contained variables calculated based on the mean and standard deviation. Each row of the dataset is an average of each activity type for all subjects.

The code was written based on the instruction of this assignment

Read training and test dataset into R environment. Read variable names into R envrionment. Read subject index into R environment.

Merges the training and the test sets to create one data set. Use command rbind to combine training and test set
Extracts only the measurements on the mean and standard deviation for each measurement. Use grep command to get column indexes for variable name contains "mean()" or "std()"
Uses descriptive activity names to name the activities in the data set Convert activity labels to characters and add a new column as factor
Appropriately labels the data set with descriptive variable names. Give the selected descriptive names to variable columns
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. Use pipeline command to create a new tidy dataset with command group_by and summarize_each in dplyr package