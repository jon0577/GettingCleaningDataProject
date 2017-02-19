##setup the packages
##install.packages("data.table")
library(data.table)
##library(reshape2)
library(dplyr)
setwd("C:/Jonathan/Personal/AnalyticsTrainingFiles/Corsera/GettingCleaningData/project")
##read activity labels which matches the activity class number to activity class label
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
##read features which has the labels for each of the data in the second column
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

## read the X_test and X_train data set
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
## read the y_test file which has the activity class number for each data row
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
## read the subject_test/train files which has the subject person number for each row
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

##add in the names for the columns
names(X_test) = features
names(X_train) = features
names(y_test) = "activity_number"
names(y_train) = "activity_number"
names(subject_test) = "subject_number"
names(subject_train) = "subject_number"
names(activity_labels) = c("activity_number", "activity_label")

##merge activity label with activity number
y_test<-merge(y_test, activity_labels)
y_train<-merge(y_train, activity_labels)

##Merge the X_test/train data with it's Y_test/train activity labels
test_data<-cbind(X_test, y_test, subject_test)
train_data<-cbind(X_train, y_train, subject_train)

##Merge the test and training data
full_data<-rbind(test_data, train_data)

##Create a logical filter for which colums to keep, which are those that are mean, stdev
##and the activity label and activity class columms
extract_features <- grepl("mean|std|subject|activity_label", names(full_data))

##filter out all columns except those for mean and stdev and the activity and subject
meanstddata<-full_data[,extract_features]

##replace cryptic names with full names
names(meanstddata)<-gsub("^t", "Time", names(meanstddata))
names(meanstddata)<-gsub("Acc", "Acceleration", names(meanstddata))
names(meanstddata)<-gsub("Gyro", "Gyroscope", names(meanstddata))
names(meanstddata)<-gsub("Mag", "Magnitude", names(meanstddata))
names(meanstddata)<-gsub("^f", "Frequency", names(meanstddata))
names(meanstddata)<-gsub("BodyBody", "Body", names(meanstddata))
##remove parentheses from names
names(meanstddata)<-gsub("\\()", "", names(meanstddata))

##tidy up the dataset to make taller
id_labels   = c("subject_number", "activity_label")
data_labels = setdiff(colnames(meanstddata), id_labels)
tidy_data = melt(meanstddata, id = id_labels, measure.vars = data_labels)
tidy_data <- rename(tidy_data, variable_type = variable)
tidy_data$subject_number <- as.factor(tidy_data$subject_number)

##calculate means of each variable_type
tidy_mean <- dcast(tidy_data, subject_number+activity_label ~ variable_type, mean)
write.table(tidy_mean, file = "tidy_data.txt",row.name=FALSE)