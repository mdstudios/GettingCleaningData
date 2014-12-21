
# Mark Allen - Getting and Cleaning Course 12/20/2014


# Mark Allen
# 1 Merges the training and the test sets to create one data set.

datatest.labels <- read.table("test/y_test.txt", col.names="label")

datatest.subjects <- read.table("test/subject_test.txt", col.names="subject")

  datatest.data <- read.table("test/X_test.txt")

 datatrain.labels <- read.table("train/y_train.txt", col.names="label")
datatrain.subjects <- read.table("train/subject_train.txt", col.names="subject")

 datatrain.data <- read.table("train/X_train.txt")

databind <- rbind(cbind(datatest.subjects, datatest.labels, datatest.data),cbind(datatrain.subjects, datatrain.labels, datatrain.data))

# 2 Extracts only the measurements on the mean and standard deviation for each measurement

holdfeatures <- read.table("features.txt", strip.white=TRUE, stringsAsFactors=FALSE)

holdfeatures.mean.std <- holdfeatures[grep("mean\\(\\)|std\\(\\)", holdfeatures$V2), ]

data.mean.std <- data[, c(1, 2, holdfeatures.mean.std$V1+2)]


# 3 Uses descriptive activity names to name the activities in the data set


datalabels <- read.table("activity_labels.txt", stringsAsFactors=FALSE)

data.mean.std$label <- datalabels[data.mean.std$label, 2]

# 4 Appropriately labels the data set with descriptive variable names. 

  good.colnames <- c("subject", "label", holdfeatures.mean.std$V2)

good.colnames <- tolower(gsub("[^[:alpha:]]", "", good.colnames))
colnames(data.mean.std) <- good.colnames


# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

aggr.data <- aggregate(data.mean.std[, 3:ncol(data.mean.std)], by=list(subject = data.mean.std$subject, label = data.mean.std$label), mean)

# write the data for course upload
write.table(format(aggr.data, scientific=T), "tidynew.txt", row.names=F, col.names=F, quote=2)
