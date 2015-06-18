library(pgmm)
data(olive)
olive = olive[,-1]

model<-train(Area ~ ., data=olive, method="rpart")
newdata = as.data.frame(t(colMeans(olive)))

predict(model, newdata)

model$finalModel
