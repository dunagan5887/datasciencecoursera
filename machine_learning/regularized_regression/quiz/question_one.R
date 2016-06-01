library(ElemStatLearn)
library(caret)

data(vowel.train)

data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

randomForestModel <- train(y ~ ., data = vowel.train, method = "rf")
boostedModel <- train(y ~ ., data = vowel.train, method = "gbm")

rfTestPredictions <- predict(randomForestModel, newdata=vowel.test)
gbmTestPredictions <- predict(boostedModel, newdata=vowel.test)

rfConfustionMatrix <- confusionMatrix(rfTestPredictions, vowel.test$y)
gbmConfustionMatrix <- confusionMatrix(gbmTestPredictions, vowel.test$y)

agreed <- rfTestPredictions == gbmTestPredictions
agreedPredictions <- rfTestPredictions[agreed]
agreedAnswers <- vowel.test[agreed,]$y

agreedConfusionMatrix <- confusionMatrix(agreedPredictions, agreedAnswers)


#ANSWER:

#RF Accuracy = 0.6082

#GBM Accuracy = 0.5152

#Agreement Accuracy = 0.6361

