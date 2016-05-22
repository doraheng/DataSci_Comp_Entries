train<-read.csv("train.csv")
head(train)
test<-read.csv("test.csv")
head(test)
submission<-read.csv('BloodDonationSubmissionFormat.csv')


colnames(train)[6]<-"Result"
colnames(train)[2]<-"Months"
colnames(train)[3]<-"Donations"
colnames(train)[4]<-"Volume"
colnames(train)[5]<-"Month_First"

colnames(test)[6]<-"Result"
colnames(test)[2]<-"Months"
colnames(test)[3]<-"Donations"
colnames(test)[4]<-"Volume"
colnames(test)[5]<-"Month_First"

model<-glm(Result~Months+Donations+Volume, family= binomial(("logit")), data=train)

summary(model)

Result<-predict(model,test, type="response")
file<-data.frame(submission[,], Result=Result)


write.csv(file,file="submit.csv",row.names = FALSE)

