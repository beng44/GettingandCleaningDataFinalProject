X_test <- read.table("test/X_test.txt", quote="\"")
X_train <- read.table("train/X_train.txt", quote="\"")

y_test <- read.table("test/y_test.txt", quote="\"")
y_train <- read.table("train/y_train.txt", quote="\"")

combo.x<-rbind(X_train,X_test)
combo.y<-rbind(y_train,y_test)

activity_labels <- read.table("activity_labels.txt", quote="\"")

subject_test <- read.table("test/subject_test.txt", quote="\"")
subject_train <- read.table("train/subject_train.txt", quote="\"")
subject_total=rbind(subject_test,subject_train)

ytestlength<-nrow(y_test)
ytrainlength<-nrow(y_train)
totalylength<-ytestlength+ytrainlength

for(i in 1:totalylength){
  if (combo.y[i,1]==1){
    combo.y[i,1]<-"WALKING"
  }
  if (combo.y[i,1]==2){
    combo.y[i,1]<-"WALKING_UPSTAIRS"
  }
  if (combo.y[i,1]==3){
    combo.y[i,1]<-"WALKING_DOWNSTAIRS"
  }
  if (combo.y[i,1]==4){
    combo.y[i,1]<-"SITTING"
  }
  if (combo.y[i,1]==5){
    combo.y[i,1]<-"STANDING"
  }
  if (combo.y[i,1]==6){
    combo.y[i,1]<-"LAYING"
  }
}


combo<-cbind(subject_total,combo.x,combo.y)


features <- read.table("features.txt", quote="\"")
features2<-t(features)
features2=cbind("Subject",features2)

colnames(combo)<-features2[2,]

colnames(combo)[colnames(combo)=="NA"] <- "Activity"

subjects=c(1:30)

require(base)
meancols<-grep("mean()",colnames(combo))
stdcols<-grep("std()",colnames(combo))

newdata<-cbind(subject_total,combo.y,combo[,meancols],combo[,stdcols])
meanFreq<-grep("meanFreq()",colnames(newdata))
newdata<-newdata[,-meanFreq]

colnames(newdata)[colnames(newdata)=="V1"]<-"Subject"
colnames(newdata)[colnames(newdata)=="V1.1"]<-"Activity"

colsofmean<-combo[,meancols]
colsofstd<-combo[,stdcols]

names(combo)<-gsub("^t","Time ",names(combo))
names(combo)<-gsub("Acc","Accelerometer",names(combo))
names(combo)<-gsub("^f","Frequency ",names(combo))
names(combo)<-gsub("Gyro","Gyroscope",names(combo))
names(combo)<-gsub("Mag","Magnitute",names(combo))
names(combo)<-gsub("BodyBody","Body",names(combo))
View(combo)


TidyData=c()

for(i in  1:30){
  for (j in 1:6){
    fakevector=subset(newdata,Subject==subjects[i]& Activity==activity_labels[j,2])
    anothervector=apply(fakevector[,3:68],2,mean)
    vector1=cbind(subjects[i],activity_labels[j,2],t(anothervector))
    TidyData=rbind(TidyData,vector1)
  }
}
colnames(TidyData)[1]="Subject"
colnames(TidyData)[2]="Activity"


names(TidyData)<-gsub("^t","Time ",names(TidyData))
names(TidyData)<-gsub("Acc","Accelerometer",names(TidyData))
names(TidyData)<-gsub("^f","Frequency ",names(TidyData))
names(TidyData)<-gsub("Gyro","Gyroscope",names(TidyData))
names(TidyData)<-gsub("Mag","Magnitute",names(TidyData))
names(TidyData)<-gsub("BodyBody","Body",names(TidyData))

write.table(TidyData,file="tidydata.txt",row.name=FALSE)
