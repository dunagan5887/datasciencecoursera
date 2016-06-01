library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)

in_training_set = (segmentationOriginal$Case == "Train") 
trainingSet <- segmentationOriginal[in_training_set,]
testingSet <- segmentationOriginal[-in_training_set,]


set.seed(125)


tree <- rpart(Class ~ ., data=trainingSet)

#a. PS
#b. WS
#c. PS
#d. Not possible


#modelFit <- train(Class ~ ., method="rpart", data=segmentationOriginal)
#predictions <- predict(modelFit, newdata=testingSet)

#columns <- c("TotalIntenCh1", "FiberWidthCh1", "PerimStatusCh1", "VarIntenCh4")

#scopeSet <- testingSet[,columns]

#quizSet <- data.frame(TotalIntenCh1 = c(23000,50000,57000,1000), FiberWidthCh1 = c(10,10,8,8), PerimStatusCh1 = c(2,0,0,2), VarIntenCh4 = c(100,100,100,100))

#quizAnswers <- predict(modelFit, newdata=quizSet)
