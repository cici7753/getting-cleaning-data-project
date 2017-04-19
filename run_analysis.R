library(dplyr)
######################################################################################################
##1. Merges the training and the test sets to create one data set.

##Download the file to destination
if(!file.exists("./cici/run_analysis.zip")){
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        f <- file.path("./cici", "run_analysis.zip")
        download.file(url, f)
}
##Unzip file
setwd("C:/CiCi/R Programming/cici")

if (!file.exists("UCI HAR Dataset")) { 
        unzip("run_analysis.zip") 
}




##reading feature vector:
activityLabel <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
##reading activity labels:
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
##reading training tables:
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
##reading testing tables:
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)




##assigning column names:
colnames(x_train) <- features[,2]
colnames(x_test) <- features[,2]
colnames(y_train) <- "ActivityId"
colnames(y_test) <- "ActivityId"
colnames(activityLabel) <- c("ActivityId", "ActivityName")
colnames(subject_train) <- "SubjectId"
colnames(subject_test) <- "SubjectId"




##merge datasets
train <- cbind(x_train, y_train, subject_train)
test <- cbind(x_test, y_test, subject_test)
##merge train and test datasets:
all <- rbind(train, test)


######################################################################################################
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
colname <- colnames(all)
colname_filtered <- grepl(".*mean.*|.*std.*", colname)|
                    grepl("ActivityId", colname)|
                    grepl("SubjectId", colname)
all_filtered <- all[, colname_filtered == TRUE]


######################################################################################################
##3. Uses descriptive activity names to name the activities in the data set
all_filtered_names <- merge(all_filtered, activityLabel,
                            by = "ActivityId",
                            all.x = TRUE) ##LEFT JOIN all_filtered

######################################################################################################
##4. Appropriately labels the data set with descriptive variable names.
finalColname <- colnames(all_filtered_names)
finalColname = gsub("[Bb]ody[Bb]ody|[Bb]ody","Body",finalColname)
finalColname = gsub("[Gg]ravity","Gravity",finalColname)
finalColname = gsub("[Gg]yro","Gyro",finalColname)
finalColname = gsub("[Aa]cc","Acc",finalColname)
finalColname = gsub("\\()","",finalColname)
finalColname = gsub("-mean","Mean",finalColname)
finalColname = gsub("-std","StdDev",finalColname)
finalColname = gsub("^t","time",finalColname)
finalColname = gsub("^f","freq",finalColname)
colnames(all_filtered_names) = finalColname

######################################################################################################
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
##variable for each activity and each subject.
newdata <- aggregate(all_filtered_names, 
                     by = list(all_filtered_names$SubjectId,
                               all_filtered_names$ActivityId,
                               all_filtered_names$ActivityName),
                     FUN = mean,
                     na.rm = TRUE)
##remove old column names
nd <- newdata[,-c(4,84,85)]
##change column names
colnames(nd)[c(1,2,3)] = c("SubjectId","ActivityId","ActivityName")
##adjust column positions
nd <- nd[,c(2,3,1,4:82)]
##sort by ActivityId
nd <- arrange(nd,ActivityId)
