# Getting and Cleaning Data Project
Qianqian Sui

## Introduction
One of the most exciting areas in all of data science right now is wearable computing - see for example this article (http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/). Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 

The purpose of this project is to collect, work with, and clean a related data set. The goal is to prepare tidy data that can be used for later analysis.

## Data Source
 A full description is available at the site where the data was obtained:
 http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## Project Steps
### Step 1: Merges the training and the test sets to create one data set.
In this step, the data sets are downloaded and unzipped. Eight files are located in "activityLable", "features", "x_train", "y_train", "subject_train", "x_test", "y_test", and "subject_test". Column names are also cleaned up in this step. After combining train and test data sets, a final data set is generated under "all".

### Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
Create a vector for defining ID, mean and standard deviation, and then filter out all the column names which are not related to mean and standard deviation. The new subset data can be found under "all_filtered_names".

## Step 3: Uses descriptive activity names to name the activities in the data set.
Merge "all_filtered_names" table with the "activityLabel" table by ActivityId in order to inlude the descriptive activity names.

## Step 4: Appropriately labels the data set with descriptive variable names.
Use gsub function to clean up the column names, including removing brackets, cleaning up upper/lower characters, refining column names, etc.

## Step 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
In this step, use aggregate function to calculate means for each variable based on each activity and each subject. After cleaning up the column names and column orders, use the write.table function to output the new data set "nd" called "newData.txt" into the destination path.
