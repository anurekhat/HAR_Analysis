## Read the training data sets
x_train <- read.table("train/X_train.txt")
subject_train <- read.table("train/subject_train.txt")
y_train <- read.table("train/y_train.txt")
## Merge the subject & activity data on the training readings and name the columns 
train <- cbind(subject_train,y_train, x_train)
colnames(train) <- c("Subject", "Activity", paste("V", 1:561, sep=""))

## Read the test data sets
x_test <- read.table("test/X_test.txt")
subject_test <- read.table("test/subject_test.txt")
y_test <- read.table("test/y_test.txt")
## Merge the subject & activity data on the test readings and name the columns 
test <- cbind(subject_test, y_test, x_test)
colnames(test) <- c("Subject", "Activity", paste("V", 1:561, sep=""))

## merge the two data sets - training & test
library(dplyr)
merged <- bind_rows(train, test)

## Read the meta data for activities & features(variables)
activity_labels <- read.table("activity_labels.txt", col.names = c("label", "activity"))
features <- read.table("features.txt", col.names=c("index","variable_name"))

## find the variable names containing mean & standard deviation measurements.
## This regular expression will pick up the last 7 angle measurements on the mean measures. 
selected_features <- filter(features, grepl("[mM]ean|std", variable_name))

## select only the required mean & std measurements. Add an offset of 2 to adjust for the 
## first two additional columns - Subject & Activity
mean_std_values <- select(merged, Subject, Activity, selected_features$index + 2)

## Set the proper variable names
colnames(mean_std_values) <- c("Subject", "Activity", as.character(selected_features$variable_name))

## Using the activity meta data, replace the activity code with the full text
mean_std_values <- mutate(mean_std_values, Activity = activity_labels$activity[Activity])
write(mean_std_values, "all_mean_std_data.txt")


## group by Subject & Activity & summarize with the mean values for all variables
smry <- mean_std_values %>%
        group_by(Subject, Activity) %>%
        summarise_each(funs(mean))

write(smry, "summary_data.txt")
