##Q1. Merges the training and the test sets to create one data set.

SubTrain<-read.table("./train/subject_train.txt", row.names=NULL)
LabelTrain<-read.table("./train/y_train.txt", row.names=NULL)
dataTrain1<-read.table("./train/X_train.txt", row.names=NULL)
Train<-dataTrain1
Train$Sub<-SubTrain
Train$Label<-LabelTrain
Train$Sub<-unlist(Train$Sub)
Train$Sub<-as.list(Train$Sub)
Train$Label<-unlist(Train$Label)
Train$Label<-as.list(Train$Label)
Train$Dis<-rep("Train",7352)

SubTest<-read.table("./test/subject_test.txt", row.names=NULL)
LabelTest<-read.table("./test/y_test.txt", row.names=NULL)
dataTest1<-read.table("./test/X_test.txt", row.names=NULL)
Test<-dataTest1
Test$Sub<-SubTest
Test$Label<-LabelTest
Test$Sub<-unlist(Test$Sub)
Test$Sub<-as.list(Test$Sub)
Test$Label<-unlist(Test$Label)
Test$Label<-as.list(Test$Label)
Test$Dis<-rep("Test",2947)

All<-rbind(as.matrix(Test),as.matrix(Train))

##2&4.Extracts only the measurements on the mean and standard deviation for each measuremen,and 
##Uses descriptive activity names to name the activities in the data set
All<-data.frame(All)
Features<-read.table("features.txt")
NameF<-as.character(Features[,2])
colnames(All)[1:561]<-NameF
## extract columes that only contains mean and std
selcols1 = grep("-mean()", names(All)) 
selcols2<-grep("-std()",names(All))
measures_m_sd<-All[,c(selcols1,selcols2)] 
## pass the subject info,feature info, and description info to the new data frame
measures_m_sd[,80:82]<-All[,562:564]

newnames <- gsub("\\(\\)", "", names(measures_m_sd))
newnames <- gsub("-", "_", newnames)
colnames(measures_m_sd)<-newnames


##3.Uses descriptive activity names to name the activities in the data set, 
activity<-read.table("activity_labels.txt")
act_c<-as.character(activity[,2])
clab = as.character(c(1:length(measures_m_sd[,1])))
 for (i in 1:6) {
  temp<- measures_m_sd$Label==i
clab[temp] = act_c[i]}
measures_m_sd$Label <- clab


##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
require(doBy)
df <- as.data.frame(lapply(measures_m_sd, unlist))
sby <- summaryBy(list(colnames(df)[1:79], c("Sub","Label")), FUN=mean, data=df)
write.table(sby, file="tinytable.txt", row.name=FALSE)


