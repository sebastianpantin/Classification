install_keras()
card.data <- read.csv("creditcard.csv",header=FALSE,sep=",",na.strings = '?')
card.data <- apply(card.data, 2, function(x) as.numeric(x))
card.data <- as.data.frame(card.data)

head(card.data,3)
card.data <- na.omit(card.data)

class_weights <- list("0"=1,"1"=984)

inTrainRows <- createDataPartition(card.data$V31,p=0.8,list=FALSE)
trainData <- data.matrix(card.data[inTrainRows,])
testData <-  data.matrix(card.data[-inTrainRows,])
x_train2 <- array_reshape(trainData[,1:30], c(nrow(trainData), 30))
y_train2 <- trainData[,31]
x_test2 <- array_reshape(testData[,1:30],c(nrow(testData), 30))
y_test2 <- testData[,31]
model_weights <- ifelse(card.data$V31 == "1",
                        (1/table(card.data$V31)[1]) * 0.5,
                        (1/table(card.data$V31)[2]) * 0.5)
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 15, input_shape = c(30)) %>% 
  layer_activation('relu') %>% 
  layer_dense(units = 1) %>% 
  layer_activation('sigmoid')

model %>% compile( 
  optimizer = optimizer_rmsprop(),
  loss = loss_binary_crossentropy,
  metrics=mean_pred
)
model %>% fit(x_train2, y_train2, epochs=5, batch_size=30, validation_split = 0.3, class_weight = class_weights)


model %>% evaluate(x_test2, y_test2)
model %>% predict_classes(x_test2)



