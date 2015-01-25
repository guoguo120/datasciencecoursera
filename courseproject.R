##Q1. Merges the training and the test sets to create one data set.

SubTrain<-read.table("subject_train.txt", row.names=NULL)
LabelTrain<-read.table("y_train.txt", row.names=NULL)
dataTrain1<-read.table("X_train.txt", row.names=NULL)
Train<-dataTrain1
Train$Sub<-SubTrain
Train$Label<-LabelTrain
Train$Sub<-unlist(Train$Sub)
Train$Sub<-as.list(Train$Sub)
Train$Label<-unlist(Train$Label)
Train$Label<-as.list(Train$Label)
Train$Dis<-rep("Train",7352)

SubTest<-read.table("subject_test.txt", row.names=NULL)
LabelTest<-read.table("y_test.txt", row.names=NULL)
dataTest1<-read.table("X_test.txt", row.names=NULL)
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

##3.Uses descriptive activity names to name the activities in the data set, 
activity<-read.table("activity_labels.txt")
act_c<-as.character(activity[,2])
clab = as.character(c(1:length(measures_m_sd[,1])))
 for (i in 1:6) {
  temp<- measures_m_sd$Label==i
clab[temp] = act_c[i]}
measures_m_sd$Label <- clab



