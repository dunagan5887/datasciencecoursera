library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

#randomForestModel <- train(y ~ ., method="rf", data=vowel.train)

randomForestModel <- randomForest(y ~ ., data = vowel.train)

variableImportance <- varImp(randomForestModel)

#Answer: x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10