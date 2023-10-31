library(dplyr)


zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

data <- "UCI HAR Dataset"
if (!file.exists(data)) {
  unzip(zipFile)
}

trainingSubjects <- read.table(file.path(data, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(data, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(data, "train", "y_train.txt"))

testSubjects <- read.table(file.path(data, "test", "subject_test.txt"))
testValues <- read.table(file.path(data, "test", "X_test.txt"))
testActivity <- read.table(file.path(data, "test", "y_test.txt"))

features <- read.table(file.path(data, "features.txt"), as.is = TRUE)

activities <- read.table(file.path(data, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

activityz <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

colnames(activityz) <- c("subject", features[, 2], "activity")

columns <- grepl("subject|activity|mean|std", colnames(activityz))

activityz <- activityz[, columns]

activityz$activity <- factor(activityz$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

activityzCol <- colnames(activityz)

activityzCol <- gsub("[\\(\\)-]", "", activityzCol)
activityzCol <- gsub("std", "StandardDeviation", activityzCol)
activityzCol <- gsub("Freq", "Frequency", activityzCol)
activityzCol <- gsub("^f", "frequencyDomain", activityzCol)
activityzCol <- gsub("^t", "timeDomain", activityzCol)
activityzCol <- gsub("Acc", "Accelerometer", activityzCol)
activityzCol <- gsub("Gyro", "Gyroscope", activityzCol)
activityzCol <- gsub("Mag", "Magnitude", activityzCol)
activityzCol <- gsub("mean", "Mean", activityzCol)
activityzCol <- gsub("BodyBody", "Body", activityzCol)

colnames(activityz) <- activityzCol

activityzMeans <- activityz %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

activityzMeans <- aggregate(. ~ subject + activity, data = activityz, FUN = mean)

write.table(activityzMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)


