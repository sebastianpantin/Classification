library(GGally)

library(klaR)
library(caret)
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

inTrain<-createDataPartition(dataToUse$Class,p=3/4,list=FALSE)
head(dataToUse)

ctrl<-trainControl(method="repeatedcv",repeats=2,summaryFunction=multiClassSummary)


### PDA
pdafit<-train(Class~.,data=dataToUse[inTrain,],method="pda",tuneLength=15,trControl=ctrl)
plot(pdafit)
pp<-predict(pdafit, newdata=dataToUse[-inTrain,],type="raw")
table(pp,dataToUse$Class[-inTrain])
### LDA
ldafit<-train(Class~.,data=dataToUse[inTrain,],method="lda",tuneLength=15,trControl=ctrl)
lp<-predict(ldafit, newdata=dataToUse[-inTrain,-31],type="raw")
table(lp,dataToUse$Class[-inTrain])
### QDA
qdafit<-train(Class~.,data=dataToUse[inTrain,],method="qda",tuneLength=15,trControl=ctrl)
qp<-predict(qdafit, newdata=dataToUse[-inTrain,-31],type="raw")
table(qp,dataToUse$Class[-inTrain])
### MDA
mdafit<-train(Class~.,data=dataToUse[inTrain,],method="mda",tuneLength=15,trControl=ctrl)
mp<-predict(mdafit, newdata=dataToUse[-inTrain,-31],type="raw")
table(mp,dataToUse$Class[-inTrain])
### NB
NBfit<-train(Class~.,data=dataToUse[inTrain,],method="nb",tuneLength=15,trControl=ctrl)
NBp<-predict(NBfit,newdata=dataToUse[-inTrain,-31],type="raw")
table(NBp,dataToUse$Class[-inTrain])

### cart
cartfit<-train(Class~.,data=dataToUse[inTrain,],method="rpart",tuneLength=15,trControl=ctrl)
cartp<-predict(cartfit, newdata=dataToUse[-inTrain,-31],type="raw")
table(cartp,dataToUse$Class[-inTrain])

### RF
RFfit<-train(Class~.,data=dataToUse[inTrain,],method="ranger",tuneLength=4,trControl=ctrl)
RFp<-predict(RFfit, newdata=dataToUse[-inTrain,],type="raw")
table(RFp,dataToUse$Class[-inTrain])

### regLogistic
RegLogfit<-train(Class~.,data=dataToUse[inTrain,],method="regLogistic",tuneLength=15,trControl=ctrl)
RegLogfitp<-predict(RegLogfit, newdata=dataToUse[-inTrain,],type="raw")
table(RegLogfitp,dataToUse$Class[-inTrain])


