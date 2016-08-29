library(dplyr)

#READ DATA--------------

#read in variable names for features and descriptive values (activity labels) for the different activities
features_names <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

#read in training data set, training test subject id's, and training set activity labels
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

#read in test data set, test test subject id's, and test set activity labels
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

#ASSIGN COLUMN NAMES AND LEVELS-------------

#Assign descriptive column names to training and test data sets
names(x_train) <- features_names[,2]
names(x_test) <- features_names[,2]
names(subject_train) <- "subject"
names(subject_test) <- "subject"
names(y_train) <- "activity"
names(y_test) <- "activity"

#Turn "Activity" columns in y_train and y_test into factor and assign descriptive label names
y_train$activity <- as.factor(y_train[,1])
y_test$activity <- as.factor(y_test[,1])
levels(y_train$activity) <- activity_labels[,2]
levels(y_test$activity) <- activity_labels[,2]


#COMBINE DATA SETS------------------

#Combine all separate trainings datasets into single dataset starting with
#dimensions and then adding measure columns
training_set <- cbind(subject_train,y_train,x_train)

#Do the same for the test set
test_set <- cbind(subject_test,y_test,x_test)

#Combine training and test set into one dataset
complete_set <- rbind(training_set,test_set)

#SUBSETTING---------------

#Subset to "Subject ID", "Activity ID" and mean and standard deviation values. 
#As per description of the assignment, MeanFreq does not need to be excluded
mean_cols_ids <- grep("-mean()",colnames(complete_set))
std_cols_ids <- grep("-std()",colnames(complete_set))

complete_set <- complete_set[,c(1,2,mean_cols_ids,std_cols_ids)]

#DERIVE NEW DATASET WITH AVERAGES PER SUBJECT AND ACTIVITY---------

average_by_subject_activity <- aggregate(complete_set[,-c(1,2)],by=list(complete_set[,1],complete_set[,2]),mean)

#Rename first two columns of new dataset to "subject" and "activity" and reassign activity labels
colnames(average_by_subject_activity)[1:2] <- c("subject","activity")
average_by_subject_activity$activity <- as.factor(average_by_subject_activity[,2])
levels(average_by_subject_activity$activity) <- activity_labels[,2]

#Change variable names to reflect that values are now averages
colnames(average_by_subject_activity)[3:length(average_by_subject_activity)] <- paste("Average",colnames(average_by_subject_activity)[3:length(average_by_subject_activity)],sep="")


write.csv(average_by_subject_activity,'tidy_data.csv')