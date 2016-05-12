
library(caret);
source('./utilities.R')

na_values = c('NA','','#DIV/0!')
armBandData = read.csv('pml-training.csv', na.strings=na_values)
TESTarmBandData = read.csv('pml-testing.csv', na.strings=na_values)




#fullArmBandDataRows = getRowsWithFullData(armBandData)
nonNaArmBandData <- removeColumnsThatAreMostlyNA(armBandData,na_ratio_threshold=0.0)
variantArmBandData <- filterOutNonVariantColumns(nonNaArmBandData)
#variantArmBandData <- convertUsernameToBinaryVariables(variantArmBandData)
relevantArmBandData <- filterOutIrrelevantVariantColumns(variantArmBandData)


# Stochastic Gradient Boosting
modelFit <- train(classe ~.,data=relevantArmBandData, method="gbm")

confusionMatrix(relevantArmBandData$classe,predict(modelFit,relevantArmBandData))

confusionMatrix(TESTarmBandData$classe,predict(modelFit,TESTarmBandData))

