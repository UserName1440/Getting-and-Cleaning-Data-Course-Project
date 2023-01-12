library(tidyr);library(dplyr)
## Set working directory
setwd("/Users/UserName1440/Desktop/Coursera/Data Science(JHU)/Getting and Cleaning Data")

#Download UCI data files from the web, unzip them, and specify time/date settings
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
Files <- "CourseDataset.zip"
if (!file.exists(Files)){
  download.file(URL, destfile = Files, mode='wb')
}
if (!file.exists("./UCI HAR Dataset")){
  unzip(Files)
}
dateDownloaded <- date()
setwd("./UCI HAR Dataset")

## Reading Activity files
ActivityLabels <- read.table("./activity_labels.txt", header = F)
YTest <- read.table("./test/y_test.txt", header = F)
YTrain <- read.table("./train/y_train.txt", header = F)
Activity <- rbind(YTest,YTrain) %>% 
  left_join(ActivityLabels,by = "V1")
colnames(Activity) <- c("ActivityID","Activity")

## Read features files
Features <- read.table("./features.txt", header = F)
XTest <- read.table("./test/X_test.txt", header = F)
XTrain <- read.table("./train/X_train.txt", header = F)

## Read subject files
SubjectTest <- read.table("./test/subject_test.txt", header = F)
SubjectTrain <- read.table("./train/subject_train.txt", header = F)

## Combining all test and train data
Testdf <- cbind(SubjectTest,XTest)
Traindf <- cbind(SubjectTrain,XTrain)
tidy <- rbind(Testdf,Traindf)
tidy <- cbind(Activity$Activity,tidy)
colnames(tidy) <- c("Activity","Subject",Features[[2]])

## Extract data for mean and standard deviation to constructure tidy table
Features2 <- unlist(sapply(c("mean","std"),grep,Features[[2]]))
Features <- Features[[2]][Features2]
tidy <- select(tidy,c("Subject","Activity",Features))

## More descriptive activity names
pattern <- c("^t","^f","Acc","Gyro","Mag","BodyBody")
replacement <- c("time","frequency","Accelerometer",
                 "Gyroscope","Magnitude","Body")
C <- length(pattern)
while(C > 0){
  colnames(tidy) <- gsub(pattern[C],replacement[C],colnames(tidy))
  C <- C-1
}

## Tidy Table with average means and standard deviations 
FinalTidy<-aggregate(.~Subject + Activity, tidy, mean)

#Save this tidy dataset to local file
write.table(FinalTidy, file = "tidydata.txt",row.name=FALSE)
