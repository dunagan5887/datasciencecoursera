library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

randomForestModel <- train(y ~ ., method="rf", data=vowel.train)

variableImportance <- varImp(randomForestModel)