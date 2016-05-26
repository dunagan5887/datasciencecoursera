library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

modelFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method="glm", family="binomial", data=trainSA)



missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

training_actual = trainSA$chd
testing_actual = testSA$chd

training_prediction = predict(modelFit, newdata=trainSA)
testing_prediction = predict(modelFit, newdata=testSA)

training_miss_class <- missClass(training_actual, training_prediction)
testing_miss_class <- missClass(testing_actual, testing_prediction)

#Test Set Misclassification: 0.31

#Training Set: 0.27

