library(dplyr)

##download and unzip the data:
#temp <- tempfile()
#download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", temp)
#unzip(temp)
#unlink(temp)
#rm(temp)

##read files into R:
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")
training_set <- read.table("UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("UCI HAR Dataset/train/y_train.txt")
training_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
test_set <- read.table("UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")
test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

##label training set:
colnames(training_set) <- features$V2
training_set$subject <- training_subjects$V1
training_set$activity <- training_labels$V1
training_set <- merge(training_set, activity_labels, by.x="activity", by.y="V1")
training_set <- training_set[,c(563,564, 1:562)]
training_set <- select(training_set, -activity)
colnames(training_set)[2] <- "activity"

##label test set:
colnames(test_set) <- features$V2
test_set$subject <- test_subjects$V1
test_set$activity <- test_labels$V1
test_set <- merge(test_set, activity_labels, by.x="activity", by.y="V1")
test_set <- test_set[,c(563,564, 1:562)]
test_set <- select(test_set, -activity)
colnames(test_set)[2] <- "activity"

##combine training set and test data set:
full_set <- rbind(training_set, test_set)
full_set2 <- tbl_df(full_set)

##create index for mean and standard deviation only:
means <- grep("mean[^A-Z]", colnames(full_set2))
stds <- grep("std[^A-Z]", colnames(full_set2))
index <- c(1:2, means, stds)
index_asc <- sort(index)

##subset only the means and standard deviations:
full_set3 <- select(full_set2, index_asc)

##create tidy data set with average of each variable 
##for each activity and each subject:
grouped <- group_by(full_set3, subject, activity)
summary <- summarise_each(grouped, funs(mean))

##clean up the variable names
names <- gsub("-", "", colnames(summary))
names2 <- gsub("\\()", "", names)
names3 <- gsub("mean", "Mean", names2)
names4 <- gsub("std", "Std", names3)
colnames(summary) <- names4
##create tidyData.txt
write.table(summary, file="tidyData.txt", row.name=FALSE)
