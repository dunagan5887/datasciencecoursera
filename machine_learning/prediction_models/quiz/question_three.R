library(pgmm)
library(tree)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

treeModel <- tree(Area ~ ., data=olive)

fitModel <- train(Area ~ ., method="rpart", data=olive)

somePrediction <- predict(fitModel, newdata=newdata)

## 2.783. It is strange because Area should be a qualitative variable - but tree is 
#  reporting the average value of Area as a numeric variable in the leaf predicted for 
#  newdata