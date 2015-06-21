trainingData <- read.csv("data/pml-training.csv",na.strings = c("NA", ""))
testingData <- read.csv("data/pml-testing.csv", na.strings = c("NA", ""))
 
library(caret)

myDataNZV <- nearZeroVar(trainingData)
trainingData <- trainingData[,-myDataNZV]
testingData <- testingData[,-myDataNZV]

nacolumns <- (colSums(is.na(trainingData)) == 0)
trainingData <- trainingData[, nacolumns]
testingData <- testingData[, nacolumns]

otherColumnsToIgnore <- grepl("X|user_name|timestamp|new_window", colnames(trainingData))
trainingData <- trainingData[, !otherColumnsToIgnore]
testingData <- testingData[, !otherColumnsToIgnore]


inTrain <- createDataPartition(y=trainingData$classe, p=0.6, list=FALSE)
myTraining <- trainingData[inTrain, ]; myTesting <- trainingData[-inTrain, ]
dim(myTraining); dim(myTesting)
library(randomForest)
modFitB1 <- randomForest(classe ~. , data=myTraining)
predictionsB1 <- predict(modFitB1, myTesting, type = "class")
confusionMatrix(predictionsB1, myTesting$classe)

predictionsB2 <- predict(modFitB1, testingData, type = "class")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)


