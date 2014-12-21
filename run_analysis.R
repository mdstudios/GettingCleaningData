
# Mark Allen - Getting and Cleaning Course 12/20/2014


# 1. Merges the training and the test sets to create one data set.
  
temptable1 <- read.table("train/X_train.txt")

temptable2 <- read.table("test/X_test.txt")

myres <- rbind(temptable1, temptable2)

temptable1 <- read.table("train/subject_train.txt")

temptable2 <- read.table("test/subject_test.txt")

myres2 <- rbind(temptable1, temptable2)

temptable1 <- read.table("train/y_train.txt")

temptable2 <- read.table("test/y_test.txt")

myres3 <- rbind(temptable1, temptable2)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 


features <- read.table("features.txt")

good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])

  myres <- myres[, good_features]

names(myres) <- features[good_features, 2]


  names(myres) <- gsub("\\(|\\)", "", names(myres))

  names(myres) <- tolower(names(myres)) 


# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")

  activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))

  myres3[,1] = activities[myres3[,1], 2]

names(myres3) <- "activity"


# 4. Appropriately labels the data set with descriptive variable names.


names(myres2) <- "subject"

cleaned <- cbind(myres2, myres3, myres)

  write.table(cleaned, "merged_data.txt")


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


countActivities = length( activities[,1])

countSubjects = length( unique(myres2)[,1])

 uniqueSubjects = unique(myres2)[,1]

 numCols = dim(cleaned)[2]

  result = cleaned[1:(countSubjects * countActivities), ]



# loop subjects & activities 

rowcount = 1

for (s in 1:countSubjects ) {
  
  for (a in 1:countActivities) {
    
     result[rowcount, 1] = countSubjects[s]
    
      result[rowcount, 2] = activities[a, 2]
    
      temp1 <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    
  result[rowcount, 3:numCols] <- colMeans(temp1[, 3:numCols])
    
    rowcount = rowcount+1
    
  }
}

write.table(result, row.names=FALSE,"ClassProject.txt")

