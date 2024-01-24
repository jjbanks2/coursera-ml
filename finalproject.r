#source file
library(dplyr)
#load data
train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")


#delete columns that relate to subject, time, summary statistics
train1<-train[,-c(1:7, 12:36, 49:59, 69:83, 87:101, 103:112, 125:139, 141:150)]
test1<-test[,-c(1:7, 12:36, 49:59, 69:83, 87:101, 103:112, 125:139, 141:150, 160)]

#split cleaned training set into training set and validation set
tsplit <- createDataPartition(train1$classe, p=0.70, list=F)
trainData <- train1[tsplit, ]
valData <- train1[-tsplit, ]

#fit model on training set
controlRf <- trainControl(method="cv", 5)
fit <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf)

#check accuracy on validation set
pred<-predict(fit, valData)
cf<-confusionMatrix(table(pred, valData$classe))
# accuracy
cf$overall
# out of sample error
1-cf$overall[1]

#apply to test data
testpred<-predict(fit, test1)