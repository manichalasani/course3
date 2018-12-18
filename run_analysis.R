library("dplyr")

library("data.table")

#read test data
readings_test<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/test/X_test.txt")
activities_test<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/test/y_test.txt")
subjects_text<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/test/subject_test.txt")

#read training data
readings_training<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/train/X_train.txt")
activities_training<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/train/y_train.txt")
subjects_training<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/train/subject_train.txt")

#read activity labels
activity_labels<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/activity_labels.txt")
activity_labels<-as.data.frame(activity_labels)
colnames(activity_labels)<-c("Activity","Activities")

#read features
features<-fread("C:/Users/manichalasani/Desktop/cOursera/R/UCI HAR Dataset/features.txt")
features<-as.data.frame(features)
colnames(features)<-c("column","name")

#STEP1: Merges the training and the test sets to create one data set
trainingdata<-cbind.data.frame(subjects_training,activities_training,readings_training)
testdata<-cbind.data.frame(subjects_text,activities_test,readings_test)
activitydata<-rbind.data.frame(trainingdata,testdata)
colnames(activitydata)<-c("Subjects","Activity",features$name)
activitydata<-as.data.frame(activitydata)
#STEP2: Identify measurements on mean and standard deviation
columnstokeep<-grepl("Subject|Activity|mean|std", colnames(activitydata))
activitydata<-activitydata[,columnstokeep]

#STEP3: Uses descriptive activity names to name the activities
activitydata<-merge(activitydata,activity_labels, by= "Activity",sort=FALSE)

activitydata<-activitydata[c(ncol(activitydata),2:(ncol(activitydata)-1))]

#STEP4: Rename variables with decriptive names
  # To remove () from the label names
columnnamesdata<-colnames(activitydata)
columnnamesdata<-gsub("\\(\\)","",columnnamesdata)
  #To expand abbrevations
columnnamesdata<-gsub("^f","frequency domain",columnnamesdata)
columnnamesdata<-gsub("^t","time domain",columnnamesdata)
columnnamesdata<-gsub("Acc","accelerometer",columnnamesdata)
columnnamesdata<-gsub("Gyro","gyroscope",columnnamesdata)
columnnamesdata<-gsub("[Mm]ag","magnitude",columnnamesdata)
columnnamesdata<-gsub("std","standard deviation",columnnamesdata)
colnames(activitydata)<-columnnamesdata

#STEP5:To calculate average of each variable each activity and each subject
activitydata<-group_by(activitydata,Activities, Subjects)
activitydata_means<-summarise_all(activitydata,funs(mean))

#STEP6: Creating a tidy data set
write.table(activitydata_means,"tidayactivitydata.txt",row.names = FALSE)
