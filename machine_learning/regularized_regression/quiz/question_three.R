set.seed(3523)
library(caret)
library(ElemStatLearn)
library(MASS)
library(lasso2)
library(AppliedPredictiveModeling)
library(elasticnet)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

# 5-fold cross-validation
K <- 5

folds <- rep(1:K, ceiling(nrow(training)/K))[1:nrow(training)]
folds <- sample(folds)

getrss <- function(y, yhat) {mean((y-yhat)^2)}
y <- training$CompressiveStrength

rss <- matrix(0, nr=K, nc=1)
colnames(rss) <- c("lasso")

# lasso

for (i in 1:K) {
  fit <- l1ce(CompressiveStrength ~ ., data=training[folds!=i,], bound=seq(0.05,1,0.05))
  gcvres <- gcv(fit)
  best.bound <- gcvres[which.min(gcvres[,"gcv"]), "rel.bound"]
  
  fit <- l1ce(CompressiveStrength ~ ., data=training[folds!=i,], bound=best.bound)
  yhat <- predict(fit, newdata=training[folds==i,])
  rss[i] <- getrss(y[folds==i], yhat)
}

compressiveStrengthIndex <- grep("CompressiveStrength", colnames(training))

predictorsDataFrame <- training[-compressiveStrengthIndex]
predictorsMatrix <- as.matrix(predictorsDataFrame)
enetObject <- enet(predictorsMatrix, training$CompressiveStrength, lambda = 0)

#Answer: IS NOT CoarseAggregate
# Answer: IS NOT Water
# Answer Cement
