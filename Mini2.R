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
iu<-sample(seq(1,nbrNegData),10000)
subedData <- negativeData[iu,]  #25 26 27 28 29 30 31
dataNumeric <- apply(subedData,2, function(x) as.numeric(paste(x)))
dataFrame <- as.data.frame(dataNumeric)

dataToUse = rbind(positiveData, dataFrame)
newfac<-rep(0,dim(dataToUse)[1])
newfac[dataToUse$Class==0]<-"OK"
newfac[dataToUse$Class==1]<-"Fraud"
newfac<-as.factor(newfac)
dataToUse$Class<-newfac

inTrain<-createDataPartition(dataToUse$Class,p=3/4,list=FALSE)
head(dataToUse)

ctrl<-trainControl(method="repeatedcv",repeats=2,summaryFunction=multiClassSummary)



pdafit<-train(Class~.,data=dataToUse[inTrain,],method="pda",tuneLength=15,trControl=ctrl)
plot(pdafit)
pp<-predict(pdafit, newdata=dataToUse[-inTrain,],type="raw")
table(pp,dataToUse$Class[-inTrain])
###



