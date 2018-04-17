install_keras()
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart.data$num[heart.data$num > 0] <- 1
head(heart.data,3)
heart.data <- na.omit(heart.data)
inTrainRows <- createDataPartition(heart.data$num,p=0.8,list=FALSE)
trainData <- data.matrix(heart.data[inTrainRows,])
testData <-  data.matrix(heart.data[-inTrainRows,])
x_train2 <- array_reshape(trainData[,1:13], c(nrow(trainData), 13))
y_train2 <- trainData[,14]
x_test2 <- array_reshape(testData[,1:13],c(nrow(testData), 13))
y_test2 <- testData[,14]
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 6, input_shape = c(13)) %>% 
  layer_activation('relu') %>% 
  layer_dense(units = 1) %>% 
  layer_activation('sigmoid')

model %>% compile( 
  optimizer = "rmsprop",
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)
model %>% fit(x_train2, y_train2, epochs=50, batch_size=6, validation_split = 0.3)
model %>% evaluate(x_test2, y_test2)
model %>% predict_classes(x_test2)
