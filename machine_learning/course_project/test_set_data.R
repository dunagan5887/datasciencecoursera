library(caret);
source('./utilities.R')
# Set random seed number
set.seed(123)
# Declare values which should be treated as na
na_values = c('NA','','#DIV/0!')
# Import the data from the file
armBandData = read.csv('pml-training.csv', na.strings=na_values)
# Remove columns which are mostly NA
nonNaArmBandData <- removeColumnsThatAreMostlyNA(armBandData)
# Remove columns which have low variance, meaning they offer little information gain
variantArmBandData <- filterOutNonVariantColumns(nonNaArmBandData)
# Convert the username column into six columns representing binary variables
# This allows the data point to be included in Principal Component Analysis
convertedArmBandData <- convertUsernameToBinaryVariables(variantArmBandData)
# Remove columns that don't appear to be relevant to the class
relevantArmBandData <- filterOutIrrelevantVariantColumns(convertedArmBandData)

inTrain <- createDataPartition(y=relevantArmBandData$classe, p=0.8, list=FALSE)

trainingSet <- relevantArmBandData[inTrain,]
testingSet <- relevantArmBandData[-inTrain,]

# Generate predictions for the actual test set based on the data in the training set
classe_index <- getColumnIndexByLabel("classe", trainingSet)
# We don't want to include the result variable in our Principal Component Analysis


data_to_pre_process = trainingSet[,-classe_index]
number_of_possible_predictors <- ncol(data_to_pre_process)


pcaCovariateSet <- preProcess(data_to_pre_process, method="pca", pcaComp=number_of_possible_predictors)
trainingSetPredictions <- predict(pcaCovariateSet, trainingSet[,-classe_index])
# Train our data set using our Principal Components data and a Multinomial method
armBandPredictionModel <- train(trainingSet$classe ~ ., method="gbm",data=trainingSetPredictions)

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
