
  
  #-----------------------------------------------------------importing libraries
  library(dplyr)
library(magrittr)


#-----------------------------------------------------------extract the x train data
xtrain <- "C:\\YOURFILEPATHHERE\\X_train.txt"
xt<-read.csv2(xtrain, header=F, sep="")

#extract the x train data features and clean
xtrainf <- "C:\\YOURFILEPATHHERE\\features.txt"
xtf <-read.csv2(xtrainf, sep="", header=F)
#renaming the column name of features dataset
xtf <- rename(xtf,index= V1, description= V2)
xtf <-select(xtf,description)
xtf<-tolower(xtf$description)
#cleaning the column name of features dataset
xtf<-gsub("-","_",xtf)
xtf<-gsub("\\(","",xtf)
xtf<-gsub("\\)","",xtf)
xtf<-gsub(",","-",xtf)

#-------------------------------------------------------------extract the y train data
ytrain <- "C:\\YOURFILEPATHHERE\\y_train.txt"
yt<-read.csv2(ytrain, header=F, sep="")


#-------------------------------------------------------------attaching the features to the data
names(xt) <- xtf
trainingdataaset<-cbind(yt,xt)


#-------------------------------------------------------------extract the x test data
xtest <- "C:\\YOURFILEPATHHERE\\X_test.txt"
xts<-read.csv2(xtest, header=F, sep="")

#-------------------------------------------------------------extract the y test data
ytest <- "C:\\YOURFILEPATHHERE\\y_test.txt"
yts<-read.csv2(ytest, header=F, sep="")

#-------------------------------------------------------------attaching the features to the data
names(xts) <- xtf
testdataaset<-cbind(yts,xts)


#-------------------------------------------------------------joining them to form one data set (train + test) in vertical manner
dataset<-rbind(trainingdataaset,testdataaset)
#deleting the duplicate column
dataset <- dataset[, !duplicated(colnames(dataset))]


#-------------------------------------------------------------activity label extraction and data type manipulation
lbl <- "C:\\YOURFILEPATHHERE\\activity_labels.txt"
lbl<-read.csv2(lbl, header=F, sep="")
#data type manipulation
lbl$V1<-as.character(lbl$V1)
dataset$V1 <- as.character(dataset$V1)


#-------------------------------------------------------------merging dataset by their common field V1
f<-merge(x=dataset,y=lbl,by="V1",sort=F)
# giving a more descriptive column name and arranging them
names(f)[names(f)=="V1"] <- "ActivityLabel"
names(f)[names(f)=="V2"] <- "ActivityDescription"
#rearranging the labels
f<-select(f,ActivityLabel,ActivityDescription,everything())
f$ActivityDescription <- as.character(f$ActivityDescription) 


#-------------------------------------------------------------changing the data type of each column
f1<-f %>% 
  mutate_each(funs(if(is.factor(.)) as.integer(.) else .))
str(f1)



#-------------------------------------------------------------getting the mean and std deviations measures only
cf1 <- colnames(f1)
#selecting the activity label and description plus the measures that have mean and std 
final<-f1[cf1 == "ActivityLabel" | cf1 == "ActivityDescription" | grepl("mean",cf1) |grepl("std",cf1) ]

#-------------------------------------------------------------create an independent tidy set that shows the average of each variable
final1<-final%>%
  group_by(ActivityDescription,ActivityLabel) %>%
  summarise_all(funs(mean))



fname<-file.path(getwd(),"tidyset_requirement5")
write.csv(x=final1,file=fname)