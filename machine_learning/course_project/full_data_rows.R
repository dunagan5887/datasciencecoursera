# File calculating for only rows with full data sets

library(caret);
source('./utilities.R')

seed_number <- runif(1, 1, 10000)
seed_number <- as.integer(seed_number)
set.seed(seed_number)

na_values = c('NA','','#DIV/0!')
armBandData = read.csv('pml-training.csv', na.strings=na_values)

fullArmBandDataRows = getRowsWithFullData(armBandData)
nonNaArmBandData <- removeColumnsThatAreMostlyNA(fullArmBandDataRows,na_ratio_threshold=0.0)
variantArmBandData <- filterOutNonVariantColumns(nonNaArmBandData)
#variantArmBandData <- convertUsernameToBinaryVariables(variantArmBandData)
relevantArmBandData <- filterOutIrrelevantVariantColumns(variantArmBandData)

inTrain <- createDataPartition(y=relevantArmBandData$classe, p=0.8, list=FALSE)
trainingSet <- relevantArmBandData[inTrain,]
testingSet <- relevantArmBandData[-inTrain,]


classe_index <- getColumnIndexByLabel("classe", trainingSet)
# We don't want to include the result variable in our Principal Component Analysis
preProcessedTrainingSet <- preProcess(trainingSet[,-classe_index], method="pca", pcaComp=115)
trainingSetPredictions <- predict(preProcessedTrainingSet, trainingSet[,-classe_index])
# Train our data set using our Principal Components data and a Multinomial method
armBandPredictionModel <- train(trainingSet$classe ~ ., method="multinom",data=trainingSetPredictions)
# Generate predictions for the testing set with our model
preProcessedTestingSet <- predict(preProcessedTrainingSet, testingSet[,-classe_index])
testingSetPredictions <- predict(armBandPredictionModel, newdata=preProcessedTestingSet)
testingSetPredictions



#armBandClassePredictions <- getClassePredictions(trainingSet, testingSet)
armBandPredictionResults <- confusionMatrix(testingSet$classe, testingSetPredictions)
print(armBandPredictionResults$overall[1])
