library(caret);
source('./utilities.R')

#seed_number <- runif(1, 1, 10000)
#seed_number <- as.integer(seed_number)
set.seed(23232)

na_values = c('NA','','#DIV/0!')
armBandData = read.csv('pml-training.csv', na.strings=na_values)
nonNaArmBandData <- removeColumnsThatAreMostlyNA(armBandData)
variantArmBandData <- filterOutNonVariantColumns(nonNaArmBandData)
convertedArmBandData <- convertUsernameToBinaryVariables(variantArmBandData)
relevantArmBandData <- filterOutIrrelevantVariantColumns(convertedArmBandData)

inTrain <- createDataPartition(y=convertedArmBandData$classe, p=0.8, list=FALSE)

trainingSet <- convertedArmBandData[inTrain,]
testingSet <- convertedArmBandData[-inTrain,]
# Generate predictions for the testing set based on the data in the training set
armBandClassePredictions <- getClassePredictions(trainingSet, testingSet)
# Evaluate the confusion matrix for the testing set predictions
armBandPredictionResults <- confusionMatrix(testingSet$classe, armBandClassePredictions)
