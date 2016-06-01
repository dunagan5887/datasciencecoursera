library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

# Generate the models
randomForestModel <- train(diagnosis ~ ., data=training, method="rf")
boostedModel <- train(diagnosis ~ ., data=training, method="gbm")
ldaModel <- train(diagnosis ~ ., data=training, method="lda")

# Predict for the models
randomForestPredictions <- predict(randomForestModel,testing)
randomForestConfusionMatrix <- confusionMatrix(randomForestPredictions, testing$diagnosis)
# Accuracy : 0.7683 

boostedPredictions <- predict(boostedModel,testing)
boostedConfusionMatrix <- confusionMatrix(boostedPredictions, testing$diagnosis)
# Accuracy : 0.7927 

ldaPredictions <- predict(ldaModel,testing)
ldaConfusionMatrix <- confusionMatrix(ldaPredictions, testing$diagnosis)
# Accuracy : 0.7683

# Stack the predictors
stackedPredictionsDataFrame <- data.frame(randomForestPredictions,boostedPredictions,ldaPredictions,diagnosis=testing$diagnosis)
stackedPredictionsModel <- train(diagnosis ~.,method="rf",data=stackedPredictionsDataFrame)
stackedPredictions <- predict(stackedPredictionsModel,stackedPredictionsDataFrame)
stackedConfusionMatrix <- confusionMatrix(stackedPredictions, stackedPredictionsDataFrame$diagnosis)

#Closest Answer:
# Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.


