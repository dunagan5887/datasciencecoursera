library(caret);
source('./utilities.R')

seed_number <- runif(1, 1, 10000)
seed_number <- as.integer(seed_number)
set.seed(seed_number)

na_values = c('NA','','#DIV/0!')
armBandData = read.csv('pml-training.csv', na.strings=na_values)
nonNaArmBandData <- removeColumnsThatAreMostlyNA(armBandData)
variantArmBandData <- filterOutNonVariantColumns(nonNaArmBandData)





#convertedArmBandData <- convertUsernameToBinaryVariables(variantArmBandData)
user_name_index <- getColumnIndexByLabel("user_name", variantArmBandData)
variantArmBandData <- variantArmBandData[,-user_name_index]



#relevantArmBandData <- filterOutIrrelevantVariantColumns(convertedArmBandData)
relevantArmBandData <- filterOutIrrelevantVariantColumns(variantArmBandData)








inTrain <- createDataPartition(y=relevantArmBandData$classe, p=0.8, list=FALSE)

trainingSet <- relevantArmBandData[inTrain,]
testingSet <- relevantArmBandData[-inTrain,]

classe_index <- getColumnIndexByLabel("classe", trainingSet)
preProcessedTrainingSet <- preProcess(trainingSet[,-classe_index], method="pca", pcaComp=45)
trainingSetPredictions <- predict(preProcessedTrainingSet, trainingSet[,-classe_index])
# Train our data set using our Principal Components data and a Multinomial method
armBandPredictionModel <- train(trainingSet$classe ~ ., method="multinom",data=trainingSetPredictions)
# Generate predictions for the testing set with our model
preProcessedTestingSet <- predict(preProcessedTrainingSet, testingSet[,-classe_index])
testingSetPredictions <- predict(armBandPredictionModel, newdata=preProcessedTestingSet)
testingSetPredictions

armBandPredictionResults <- confusionMatrix(testingSet$classe, testingSetPredictions)

# Accuracy : 0.6875  




TESTtestSetData = read.csv('pml-testing.csv', na.strings=na_values)
TESTnonNaArmBandData <- removeColumnsThatAreMostlyNA(TESTtestSetData)
TESTvariantArmBandData <- filterOutNonVariantColumns(TESTnonNaArmBandData)
TESTconvertedArmBandData <- convertUsernameToBinaryVariables(TESTvariantArmBandData)
TESTrelevantTestArmBandData <- filterOutIrrelevantVariantColumns(TESTconvertedArmBandData)

problem_id_index <- getColumnIndexByLabel("problem_id", TESTrelevantTestArmBandData)
TESTpreProcessedTestingSet <- predict(preProcessedTrainingSet, TESTrelevantTestArmBandData[,-problem_id_index])
TESTtestingSetPredictions <- predict(armBandPredictionModel, newdata=TESTpreProcessedTestingSet)


