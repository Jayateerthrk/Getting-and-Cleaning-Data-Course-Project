#Download the zipped file
getwd()
if(!file.exists("./data")){dir.create("./data")}
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile ="./data/Dataset.zip")

#unzip the files
unzip(zipfile ="./data/Dataset.zip",exdir="./data" )

#1. Merge the training and the test sets to create one data set.

  #Reading Training tables
  x_train<-read.table("./data/UCI HAR Dataset/train/X_train.txt")
  y_train<-read.table("./data/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  
  #Reading Testing tables
  x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  
  #Reading feature vector
  features<-read.table("./data/UCI HAR Dataset/features.txt")
  
  #Reading Activity lables
  activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')
  
  #Assign coulmn names
  colnames(x_train) <- features[,2] 
  colnames(y_train) <-"activityId"
  colnames(subject_train) <- "subjectId"
  
  colnames(x_test) <- features[,2] 
  colnames(y_test) <- "activityId"
  colnames(subject_test) <- "subjectId"
  
  colnames(activityLabels) <- c('activityId','activityType')
  
  
  #Merging in to one file
  mrg_train<-cbind(y_train,subject_train,x_train)
  mrg_test <- cbind(y_test, subject_test, x_test)  
  setAllInOne <-rbind(mrg_train,mrg_test)
  
  # 2. Extract only the measurements on the mean and standard deviation for each measurement.
  #Read column names
  colNames <- colnames(setAllInOne)
  
  #Vector for identifying ID,mean and SD
  mean_and_std <- (grepl("activityId" , colNames) | 
                     grepl("subjectId" , colNames) | 
                     grepl("mean.." , colNames) | 
                     grepl("std.." , colNames) 
                  )
  
  #Subsetting
  setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
  
  # 3. Use descriptive activity names to name the activities in the data set
  setWithActivityNames<-merge(setForMeanAndStd,activityLabels,by='activityId',all.x =TRUE)
  
  # 4. Appropriately label the data set with descriptive variable names.
  #This is already covered above
  
  # 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
  #Create 2nd tidy data set
  secTidySet <-aggregate(.~subjectId+activityId,setWithActivityNames,mean)
  secTidySet<-secTidySet[order(secTidySet$subjectId,secTidySet$activityId),]
  
  # Writing back second tidy data set in txt file
  write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
  