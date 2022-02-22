library(tidyverse)
library(reshape2)

# load the activity lables and features
act<- read_delim("./UCI HAR Dataset/activity_labels.txt", col_names = c("ID", "activity_name"))

feat_info <- read_table("./UCI HAR Dataset/features_info.txt", col_names = "feature_info")

feat <- read.table("./UCI HAR Dataset/features.txt")[,2]

# only select features that have mean or std in the title
extract <- grepl("mean|std", feat)

#  load all the training data
sub_train <- read_delim("./UCI HAR Dataset/train/subject_train.txt", col_names = "sub", delim="\t")

train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
names(train_x) = feat

# select for only the mean  and std training data
train_x = train_x[,extract]

# load the Y
train_y <- read_table("./UCI HAR Dataset/train/Y_train.txt", col_names = c("ID"))

# merge
train <- cbind(train_x, train_y) %>%
  merge(act, by="ID") %>%
  cbind(sub_train)


# do the same  for the test data

sub_test <- read_delim("./UCI HAR Dataset/test/subject_test.txt", col_names = "sub", delim="\t")

test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
names(test_x) = feat

# select for only the mean  and std testing data
test_x = test_x[,extract]

# load the Y
test_y <- read_table("./UCI HAR Dataset/test/Y_test.txt", col_names = c("ID"))

# merge
test <- cbind(test_x, test_y) %>%
  merge(act, by="ID") %>%
  cbind(sub_test)

# merge the test and the training data

all.dat <- rbind(test, train)

# make tidy data 
tidy.dat <- all.dat %>%
  gather(feature, measurement, c("tBodyAcc-mean()-X":"fBodyBodyGyroJerkMag-meanFreq()"))
  
# group by  each variable for each activity and each subject and take mean
tidy.dat2 <- tidy.dat %>%
  group_by(feature, sub, activity_name) %>%
  summarize(mean_measurement=mean(measurement))
