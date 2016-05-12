library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(ggplot2)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(Superplasticizer, data=training)


"There are values of zero so when you take the log() transform those values will be -Inf."