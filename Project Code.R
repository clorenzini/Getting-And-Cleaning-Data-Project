

setwd("C:/Users/Chris/Desktop/Coursera/Getting and Cleaning Data")

test<-read.delim("X_test.txt",sep="",header=F)
train<-read.delim("X_train.txt",sep="",header=F)
testlab<-read.delim("y_test.txt",header=F)
trainlab<-read.delim("y_train.txt",header=F)
testsub<-read.delim("subject_test.txt",header=F)
trainsub<-read.delim("subject_train.txt",header=F)
features<-read.delim("features.txt",header=F)


for(i in 1:length(testlab[,1])){
  if(testlab[i,1]==1){testlab[i,1]<-"WALKING"}
  if(testlab[i,1]==2){testlab[i,1]<-"WALKING_UPSTAIRS"}
  if(testlab[i,1]==3){testlab[i,1]<-"WALKING_DOWNSTAIRS"}
  if(testlab[i,1]==4){testlab[i,1]<-"SITTING"}
  if(testlab[i,1]==5){testlab[i,1]<-"STANDING"}
  if(testlab[i,1]==6){testlab[i,1]<-"LAYING"}
}

for(i in 1:length(trainlab[,1])){
  if(trainlab[i,1]==1){trainlab[i,1]<-"WALKING"}
  if(trainlab[i,1]==2){trainlab[i,1]<-"WALKING_UPSTAIRS"}
  if(trainlab[i,1]==3){trainlab[i,1]<-"WALKING_DOWNSTAIRS"}
  if(trainlab[i,1]==4){trainlab[i,1]<-"SITTING"}
  if(trainlab[i,1]==5){trainlab[i,1]<-"STANDING"}
  if(trainlab[i,1]==6){trainlab[i,1]<-"LAYING"}
}


data<-rbind(test,train)
labs<-rbind(testlab,trainlab)

data1<-cbind(labs,data)
colnames(data1)<-c("Activity",as.character(features[,1]))

grep("mean",features[,1])
grep("std",features[,1])
a<-c(grep("mean",features[,1]),grep("std",features[,1]))
a1<-a+1
a2<-c(1,a1)
data2<-data1[,sort(a2)]

set<-features[sort(a),]

set<-as.data.frame(set)


set$Type<-""
set$Stat<-""
set$Device<-""
set$Coor<-""
set$tf<-""
set$MF<-""
set$Jerk<-""
set$label<-""

b<-grep("Body",set[,1])
c<-grep("Gravity",set[,1])
d<-grep("mean",set[,1])
e<-grep("std",set[,1])
f<-grep("Acc",set[,1])
g<-grep("Gyro",set[,1])
h<-grep("-X",set[,1])
k<-grep("-Y",set[,1])
l<-grep("-Z",set[,1])
m<-grep(" t",set[,1])
n<-grep(" f",set[,1])
o<-grep("Mag",set[,1])
p<-grep("Freq",set[,1])
q<-grep("Jerk",set[,1])

set[b,2]<-"Body "
set[c,2]<-"Gravity "
set[d,3]<-"Mean"
set[e,3]<-"SD"
set[f,4]<-"Acc "
set[g,4]<-"Gyro "
set[h,5]<-"X "
set[k,5]<-"Y "
set[l,5]<-"Z "
set[m,6]<-"T "
set[n,6]<-"F "
set[o,7]<-"Mag "
set[p,7]<-"Freq "
set[q,8]<-"Jerk "

set$label<-paste0(set$tf,set$Type,set$Device,set$Jerk,set$MF,set$Coor,set$Stat)

colnames(data2)<-c("Activity",set$label)

uact<-unique(data2$Activity)

mean<-array()
mat<-matrix(NA,nrow=length(uact),ncol=length(data2)-1)

for(i in 1:length(uact)){
  
  aa<-data2[data2$Activity==uact[i],]
  
  for(j in 1:length(data2)-1){
    
    mean[j]<-mean(aa[,j+1])
    
  }
  
  mat[i,]<-c(mean)
  
}

colnames(mat)<-set$label
rownames(mat)<-uact

write.table(mat,"Tidy Set.txt",sep="\t")



