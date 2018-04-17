library(pROC)
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
head(heart.data,3)
heart.data$num[heart.data$num > 0] <- 1
heart.data$num[heart.data$num == 0] <- "NoHD"
heart.data$num[heart.data$num == 1] <- "HD"
barplot(table(heart.data$num),
        main="Fate", col="black")
# change a few predictor variables from integer to factors (make dummies)
chclass <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")

convert.magic <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}
heart.data <- na.omit(heart.data)
heart.data <- convert.magic(heart.data,chclass)
heart = heart.data #add labels only for plot
levels(heart$num) = c("No disease","Disease")
levels(heart$sex) = c("female","male","")
mosaicplot(heart$sex ~ heart$num,
           main="Fate by Gender", shade=FALSE,color=TRUE,
           xlab="Gender", ylab="Heart disease")

B<-5
ERRMAT<-matrix(0,B,8)

ctrl<-trainControl(method="cv",summaryFunction=multiClassSummary)

for (b in (1:B)) {
  inTrainRows <- createDataPartition(heart.data$num,p=0.7,list=FALSE)
  trainData <- heart.data[inTrainRows,]
  testData <-  heart.data[-inTrainRows,-14]
  trainDataY <- heart.data$num[inTrainRows]
  testDataY <- heart.data$num[-inTrainRows]
  fit<-train(num~.,data=trainData,method="rpart",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,1]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(num~.,data=trainData,method="ranger",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,2]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(num~.,data=trainData,method="knn",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,3]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(num~.,data=trainData,method="lda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,4]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(num~.,data=trainData,method="qda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,5]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(num~.,data=trainData,method="pda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,6]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(num~.,data=trainData,method="nb",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,7]<-length(pp[pp!=testDataY])/length(pp)
  fit<-train(num~.,data=trainData,method="mda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=testData,type="raw")
  ERRMAT[b,8]<-length(pp[pp!=testDataY])/length(pp)
  print(b)
}
par(mfrow=c(1,1))
boxplot(ERRMAT,names=c("CART","RF","knn","lda","qda","pda","nb","mda"))
#
