library(caret);
source('./utilities.R')
# Set random seed number
set.seed(123)
# Declare values which should be treated as na
na_values = c('NA','','#DIV/0!')
# Import the data from the file
armBandData = read.csv('pml-training.csv', na.strings=na_values)
# Remove columns which have NA values
nonNaArmBandData <- removeColumnsThatHaveNAValues(armBandData)
# Remove columns which have low variance, meaning they offer little information gain
variantArmBandData <- filterOutNonVariantColumns(nonNaArmBandData)
# Remove columns that don't appear to be relevant to the class
relevantArmBandData <- filterOutIrrelevantVariantColumns(variantArmBandData)

inTrain <- createDataPartition(y=relevantArmBandData$classe, p=0.8, list=FALSE)

trainingSet <- relevantArmBandData[inTrain,]
testingSet <- relevantArmBandData[-inTrain,]

# Generate predictions for the actual test set based on the data in the training set
classe_index <- getColumnIndexByLabel("classe", trainingSet)
# We don't want to include the result variable in our Principal Component Analysis
data_to_predict_with = trainingSet[,-classe_index]

# Train our data set using our Principal Components data and a Multinomial method
armBandPredictionModel <- train(trainingSet$classe ~ ., method="rf",preProcess="pca",data=trainingSet)

#preProcessedTestingSet <- predict(pcaCovariateSet, testingSet[,-classe_index])
testingSetPredictions <- predict(armBandPredictionModel, newdata=data_to_predict_with)
armBandPredictionResults <- confusionMatrix(trainingSet$classe, testingSetPredictions)

TESTtestSetData = read.csv('pml-testing.csv', na.strings=na_values)
TESTnonNaArmBandData <- removeColumnsThatHaveNAValues(TESTtestSetData)
TESTvariantArmBandData <- filterOutNonVariantColumns(TESTnonNaArmBandData)
TESTrelevantTestArmBandData <- filterOutIrrelevantVariantColumns(TESTvariantArmBandData)

# Generate predictions for the testing set with our model
problem_id_index <- getColumnIndexByLabel("problem_id", TESTtestSetData)

testingSetPredictions <- predict(armBandPredictionModel, newdata=TESTrelevantTestArmBandData[,-problem_id_index])
