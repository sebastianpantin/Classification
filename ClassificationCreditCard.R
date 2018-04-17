creditcard<-read.csv("creditcard.csv")
iu<-sample(seq(1,284808),20000)
subedData <- creditcard[iu,]  #25 26 27 28 29 30 31
dataNumeric <- apply(subedData,2, function(x) as.numeric(paste(x)))
dataFrame <- as.data.frame(dataNumeric)


dataFrame$Class <- as.factor(dataFrame$Class)



positiveData <- creditcard[creditcard[,31]==1,]
positiveData <- na.omit(positiveData)
negativeData <- creditcard[creditcard[,31]==0,]
negativeData <- na.omit(negativeData)
nbrNegData <- dim(negativeData)[1]
iu<-sample(seq(1,nbrNegData),1000)
restData <- negativeData[-iu,]
subedData <- negativeData[iu,]  
dataNumeric <- apply(subedData,2, function(x) as.numeric(paste(x)))
dataFrame <- as.data.frame(dataNumeric)
dataNumeric <- apply(restData,2, function(x) as.numeric(paste(x)))
dataFrameRest <- as.data.frame(dataNumeric)


dataToUse = rbind(positiveData, dataFrame)
newfac<-rep(0,dim(dataToUse)[1])
newfac[dataToUse$Class==0]<-"OK"
newfac[dataToUse$Class==1]<-"Fraud"
newfac<-as.factor(newfac)
dataToUse$Class<-newfac
B<-5
ERRMAT<-matrix(0,B,6)

ctrl<-trainControl(method="cv",summaryFunction=multiClassSummary)

for (b in (1:B)) {
  inTrainRows <- createDataPartition(dataToUse$Class,p=0.7,list=FALSE)
  trainData <- dataToUse[inTrainRows,]
  trainData$Class <- as.factor(trainData$Class)
  testData <-  dataToUse[-inTrainRows,-31]
  trainDataY <- as.factor(dataToUse$lass[inTrainRows])
  testDataY <- as.factor(dataToUse$Class[-inTrainRows])
  fit<-train(Class~.,data=trainData,method="rpart",tuneLength=15,trControl=ctrl)
  print('fit 1')
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,1]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(Class~.,data=trainData,method="ranger",tuneLength=15,trControl=ctrl)
  print('fit 2')
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,2]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(Class~.,data=trainData,method="knn",tuneLength=15,trControl=ctrl)
  print('fit 3')
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,3]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(Class~.,data=trainData,method="lda",tuneLength=15,trControl=ctrl)
  print('fit 4')
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,4]<-length(pp[pp!=testDataY])/length(pp)
  # fit<-train(Class~.,data=trainData,method="qda",tuneLength=15,trControl=ctrl)
  # print('fit 5')
  # pp<-predict(fit,newdata=testData,type="raw")
  # ERRMAT[b,5]<- length(pp[pp!=testDataY])/length(pp)
  fit<-train(Class~.,data=trainData,method="pda",tuneLength=15,trControl=ctrl)
  print('fit 6')
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,5]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(Class~.,data=trainData,method="nb",tuneLength=15,trControl=ctrl)
  print('fit 7')
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,6]<-length(pp[pp!=testDataY])/length(pp)
  # fit<-train(Class~.,data=trainData,method="mda",tuneLength=15,trControl=ctrl)
  # print('fit 8')
  # pp<-predict(fit,newdata=testData,type="raw")
  # ERRMAT[b,7]<-length(pp[pp!=testDataY])/length(pp)
  print(b)
}
par(mfrow=c(1,1))
boxplot(ERRMAT,names=c("CART","RF","knn","lda","pda","nb"))        
