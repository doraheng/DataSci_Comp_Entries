pkgs <- c('reshape2', 'plyr', 'data.table', 'Lahman')
install.packages(pkgs)
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages('randomForest')
library(randomForest)


train<-read.csv("train.csv")
head(train)
test<-read.csv("test.csv")
head(test)
#https://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015/forums/t/13383/warning-message-in-random-forest
submission<-read.csv("sample_submission.csv")
head(submission)


ggplot(train, aes(x=OutcomeType))+geom_bar(aes(fill=OutcomeSubtype))
ggplot(train, aes(x=OutcomeType))+geom_bar()+facet_wrap(~AnimalType)
ggplot(train, aes(x=AgeuponOutcome))+geom_bar(aes(fill=OutcomeType))
ggplot(train, aes(x=SexuponOutcome))+geom_bar(aes(fill=OutcomeType))

modeltrain<-train[c(4,6:8)]
modeltest<-test[,4:6]
modeltrain$AgeuponOutcome<-factor(modeltrain$AgeuponOutcome,levels=levels(modeltest$AgeuponOutcome))
modeltest$SexuponOutcome<-factor(modeltest$SexuponOutcome,levels=levels(modeltrain$SexuponOutcome))

model<-randomForest(OutcomeType~., data=modeltrain)
summary(model)
model$importance

Result<-predict(model, modeltest)
head(Result)
file<-data.frame(test[,1],Result=Result)

file$Adoption<- ifelse(file$Result=="Adoption",1,0)
file$Died<- ifelse(file$Result=="Died",1,0)
file$Euthanasia<- ifelse(file$Result=="Euthanasia",1,0)
file$Return_to_owner<- ifelse(file$Result=="Return_to_owner",1,0)
file$Transfer<- ifelse(file$Result=="Transfer",1,0)

write.csv(file[c(1,3:7)],file="submit.csv",row.names = FALSE)

