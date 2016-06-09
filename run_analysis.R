# 0. Get data
if (!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
    download.file(fileURL, "getdata-projectfiles-UCI HAR Dataset.zip")
}  
if (!file.exists("UCI HAR Dataset")) { 
    unzip("getdata-projectfiles-UCI HAR Dataset.zip") 
}


# 1. Merges the training and the test sets to create one data set.

trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainActivityLabel <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubject, trainActivityLabel, trainData)
rm(trainData,trainActivityLabel,trainSubject)

testData <- read.table("UCI HAR Dataset/test/X_test.txt")
testActivityLabel <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubject, testActivityLabel, testData)
rm(testData,testActivityLabel,testSubject)

trainTest <- rbind(train, test)
rm(train,test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

feature <- read.table("UCI HAR Dataset/features.txt")
feature <- as.character(feature[,2])
meanSd <- grep(".*mean.*|.*std.*", feature)
trainTestSelected <- trainTest[,c(1,2,meanSd+2)]
rm(trainTest)

# 3. Uses descriptive activity names to name the activities in the data set.

activityLabel <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabel[,2] <- tolower(as.character(activityLabel[,2]))
activityLabel[,2] <- gsub("_u","U", activityLabel[,2])
activityLabel[,2] <- gsub("_d","D", activityLabel[,2])
trainTestSelected[,2] <- factor(trainTestSelected[,2],levels = activityLabel[,1],labels = activityLabel[,2])
rm(activityLabel)
trainTestSelected[,1] <- as.factor(trainTestSelected[,1])


# 4. Appropriately labels the data set with descriptive variable names.

featureSelected <- gsub("std","Std",feature[meanSd])
rm(feature,meanSd)
featureSelected <- gsub("mean","Mean",featureSelected)
featureSelected <- gsub("[()-]","",featureSelected)
colnames(trainTestSelected) <- c("subject", "activityLabel", featureSelected)
rm(featureSelected)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(reshape2)
melt <- melt(trainTestSelected, id = c("subject", "activityLabel"))
desiredData <- dcast(melt, subject + activityLabel ~ variable, mean)
rm(melt)
#rm(trainTestSelected)
write.table(desiredData, "tidy.txt", row.names = FALSE, quote = FALSE)
#rm(desiredData)
