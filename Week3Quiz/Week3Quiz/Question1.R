
loadpackages <- function(xtable){
  if(xtable %in% rownames(installed.packages()) == FALSE) 
    {install.packages(xtable)}
 library(package = xtable, character.only = TRUE )
}
                          
loadpackages("AppliedPredictiveModeling")
loadpackages("caret")
loadpackages("ElemStatLearn")
loadpackages("pgmm")
loadpackages("rpart")
loadpackages("e1071")
loadpackages("rattle")
loadpackages("rpart.plot")

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

set.seed(125)
train <- subset(segmentationOriginal, Case == "Train")
test <- subset(segmentationOriginal, Case == "Test")

modFit <- train(Class ~ ., data = train, method = "rpart")
modFit$finalModel



plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = .8)

fancyRpartPlot(modFit$finalModel)
fancyRpartPlot(modFit)


predict(modFit, newdata = train)