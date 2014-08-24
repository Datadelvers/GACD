# 1. Merges the training and the test sets to create one data set.

setwd("/media/datadelvers/datadelversdata/coursera/Data Science/3. Getting and Cleaning Data/CourseAssignment/")

MyDataTrain = read.table("UCI HAR Dataset/train/X_train.txt")
MyDataTest = read.table("UCI HAR Dataset/test/X_test.txt")

DF.MyDataTrain <- data.frame(MyDataTrain)
DF.MyDataTest <- data.frame(MyDataTest)

DF.MyDataTotal <- rbind(DF.MyDataTrain, DF.MyDataTest)

Features <- read.table("UCI HAR Dataset/features.txt")
colnames(DF.MyDataTotal) <- Features$V2

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
ColMeanOrSD <- grep("mean\\(\\)|std\\(\\)", Features[, 2])
DF.MyDataTotal <- DF.MyDataTotal[, ColMeanOrSD]

# 3. Appropriately labels the data set with descriptive variable names.
names(DF.MyDataTotal) <- gsub("-", "", names(DF.MyDataTotal))
names(DF.MyDataTotal) <- gsub("\\(\\)", "", Features[ColMeanOrSD, 2])
names(DF.MyDataTotal) <- gsub("mean", "Mean", names(DF.MyDataTotal)) 
names(DF.MyDataTotal) <- gsub("std", "Standard Deviation", names(DF.MyDataTotal))


# 4. Uses descriptive activity names to name the activities in the data set
activity <- read.table("UCI HAR Dataset/activity_labels.txt")

MyDataTrainLabel <- read.table("UCI HAR Dataset/train/y_train.txt")
MyDataTestLabel <- read.table("UCI HAR Dataset/test/y_test.txt")
MyDataTotalLabel <- rbind(MyDataTrainLabel, MyDataTestLabel)

activityLabel <- activity[MyDataTotalLabel[, 1], 2]
MyDataTotalLabel[, 1] <- activityLabel
names(MyDataTotalLabel) <- "activity"

MyDataTrainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
MyDataTestSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
MyDataTotalSubject <- rbind(MyDataTrainSubject, MyDataTestSubject)

names(MyDataTotalSubject) <- "subject"
CleanedData <- cbind(MyDataTotalSubject, MyDataTotalLabel, DF.MyDataTotal)

write.table(CleanedData, "CleanedData.txt",  row.name=FALSE) 

SubLength <- length(table(MyDataTotalSubject))
ActLength <- dim(activity)[1]
columnLen <- dim(CleanedData)[2]
result <- matrix(NA, nrow=SubLength*ActLength, ncol=columnLen)
result <- as.data.frame(result)
colnames(result) <- colnames(CleanedData)
row <- 1
for(x in 1:SubLength) {
  for(y in 1:ActLength) {
    result[row, 1] <- sort(unique(MyDataTotalSubject)[, 1])[x]
    result[row, 2] <- activity[y, 2]
    bool1 <- x == CleanedData$subject
    bool2 <- activity[y, 2] == CleanedData$activity
    result[row, 3:columnLen] <- colMeans(CleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}


result <- result[with(result, order(subject, activity)), ]

result$activity[result$activity == 1] <- "Walking"
result$activity[result$activity == 2] <- "Walking Upstairs"
result$activity[result$activity == 3] <- "Walking Downstairs"
result$activity[result$activity == 4] <- "Sitting"
result$activity[result$activity == 5] <- "Standing"
result$activity[result$activity == 6] <- "Laying"

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(result, "FinalResults.txt",  row.name=FALSE) 
