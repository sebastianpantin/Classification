library(plyr)
library(caret)
BaggingMethod <- function(dataMat, method, partitionFraction, B){
  ctrl<-trainControl(method="cv",summaryFunction=multiClassSummary)
  for (b in (1:B)) {
    inTrain<-createDataPartition(dataToUse$Class, p=partitionFraction, list=FALSE)
    fit<-train(Class~., data=dataMat[inTrain,], method=method, tuneLength=15, trControl=ctrl)
    #pp<-predict(fit, newdata=dataMat[-inTrain,], type="raw")
    #ppFrame = data.frame(pp)
    #if (b==1) {
    #  returnSelection = cbind(ppFrame)
    #} else {
    #  returnSelection = cbind(returnSelection, ppFrame)
    #}
  }
  returnSelection
}

MajorityVote <- function(classifications){
  # point on row, methods on col
  # binary count...
  classes = levels(classifications[,1])
  isFirstClass = classifications == classes[1]
  nbrMethods = dim(classifications)[2]
  meanVote = rowSums(isFirstClass) / nbrMethods
  result = factor(meanVote >= 0.5)
  mapvalues(result, from = c(FALSE, TRUE), to = c("OK", "Fraud"))
}
mpTest = data.frame(mp)
lpTest = data.frame(lp)
NBpTest = data.frame(NBp)
qpTest = data.frame(qp)
cartpTest = data.frame(cartp)
test.data = cbind(mpTest,lpTest, NBpTest, qpTest, cartpTest)

test = MajorityVote(test.data)
table(test,dataToUse$Class[-inTrain])

test

###
classBagged = BaggingMethod(dataToUse, 'qda', 3/4, 3)
majVoteQDA = MajorityVote(classBagged)

