set.seed(3523)

library(AppliedPredictiveModeling)
library(e1071)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

svmModel <- svm(CompressiveStrength ~ ., data = training)

predictions <- predict(svmModel, newdata=testing)

n <- length(predictions)

mse = sum((predictions-testing$CompressiveStrength)^2) / n
rmse <- sqrt(mse)

# Answer IS NOT 107.44
# Answer 6.72