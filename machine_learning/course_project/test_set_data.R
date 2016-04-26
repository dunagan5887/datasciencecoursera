library(caret);
source('./utilities.R')

set.seed(23232)

na_values = c('NA','','#DIV/0!')
armBandData = read.csv('pml-training.csv', na.strings=na_values)
nonNaArmBandData <- removeColumnsThatAreMostlyNA(armBandData)
variantArmBandData <- filterOutNonVariantColumns(nonNaArmBandData)
convertedArmBandData <- convertUsernameToBinaryVariables(variantArmBandData)
relevantArmBandData <- filterOutIrrelevantVariantColumns(convertedArmBandData)

inTrain <- createDataPartition(y=relevantArmBandData$classe, p=0.8, list=FALSE)

trainingSet <- relevantArmBandData[inTrain,]
testingSet <- relevantArmBandData[-inTrain,]
# Generate predictions for the testing set based on the data in the training set
#armBandClassePredictions <- getClassePredictions(trainingSet, testingSet)
# Evaluate the confusion matrix for the testing set predictions
#armBandPredictionResults <- confusionMatrix(testingSet$classe, armBandClassePredictions)

# Generate predictions for the actual test set based on the data in the training set
classe_index <- getColumnIndexByLabel("classe", trainingSet)
# We don't want to include the result variable in our Principal Component Analysis
pcaCovariateSet <- preProcess(trainingSet[,-classe_index], method="pca", pcaComp=40)
trainingSetPredictions <- predict(pcaCovariateSet, trainingSet[,-classe_index])
# Train our data set using our Principal Components data and a Multinomial method
armBandPredictionModel <- train(trainingSet$classe ~ ., method="multinom",data=trainingSetPredictions)

preProcessedTestingSet <- predict(pcaCovariateSet, testingSet[,-classe_index])
testingSetPredictions <- predict(armBandPredictionModel, newdata=preProcessedTestingSet)
armBandPredictionResults <- confusionMatrix(testingSet$classe, testingSetPredictions)




TESTtestSetData = read.csv('pml-testing.csv', na.strings=na_values)
TESTnonNaArmBandData <- removeColumnsThatAreMostlyNA(TESTtestSetData)
TESTvariantArmBandData <- filterOutNonVariantColumns(TESTnonNaArmBandData)
TESTconvertedArmBandData <- convertUsernameToBinaryVariables(TESTvariantArmBandData)
TESTrelevantTestArmBandData <- filterOutIrrelevantVariantColumns(TESTconvertedArmBandData)


# Generate predictions for the testing set with our model
problem_id_index <- getColumnIndexByLabel("problem_id", TESTtestSetData)

TESTSETpreProcessedTestingSet <- predict(pcaCovariateSet, TESTrelevantTestArmBandData[,-problem_id_index])
testingSetPredictions <- predict(armBandPredictionModel, newdata=TESTSETpreProcessedTestingSet)




