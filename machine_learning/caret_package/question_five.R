library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_column_indexes <- grep('^IL', names(training))
diagnosis_column <- grep('diagnosis', names(training))

IL_columns <- training[,c(IL_column_indexes, diagnosis_column)]

IL_columns_TESTING <- testing[,c(IL_column_indexes, diagnosis_column)]

# Predicting without PCA
modelFit <- train(diagnosis ~ .,method="glm",data=IL_columns)
confusionMatrix(testing$diagnosis,predict(modelFit,testing))

# Predicting with PCA
pcaPreProc <- preProcess(IL_columns[,-diagnosis_column],method="pca",thresh=.8)
trainPC <- predict(pcaPreProc,IL_columns[,-diagnosis_column])
pcaModelFit <- train(IL_columns$diagnosis ~ .,method="glm",data=trainPC)

testPC <- predict(pcaPreProc,IL_columns_TESTING[,-diagnosis_column])
confusionMatrix(IL_columns_TESTING$diagnosis,predict(pcaModelFit,testPC))




modelFit <- train(IL_columns$diagnosis ~ .,method="glm",data=IL_columns,preProcess="pca")

confusionMatrix(testing$diagnosis,predict(modelFit,testing$diagnosis))
