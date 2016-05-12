getClassePredictions <- function(trainingSet, testingSet)
{
    classe_index <- getColumnIndexByLabel("classe", trainingSet)
    # We don't want to include the result variable in our Principal Component Analysis
    preProcessedTrainingSet <- preProcess(trainingSet[,-classe_index], method="pca", pcaComp=10)
    trainingSetPredictions <- predict(preProcessedTrainingSet, trainingSet[,-classe_index])
    # Train our data set using our Principal Components data and a Multinomial method
    armBandPredictionModel <- train(trainingSet$classe ~ ., method="glm",data=trainingSetPredictions)
    # Generate predictions for the testing set with our model
    preProcessedTestingSet <- predict(preProcessedTrainingSet, testingSet[,-classe_index])
    testingSetPredictions <- predict(armBandPredictionModel, newdata=preProcessedTestingSet)
    testingSetPredictions
}
# This method converts a categorical covariate into 6 binary covariates, allowing the data point to be included in Principal Component Analysis
convertUsernameToBinaryVariables <- function(variantArmBandData)
{
    is_adelmo <- variantArmBandData$user_name == 'adelmo'
    variantArmBandData$is_adelmo <- as.numeric(is_adelmo)
    
    is_carlitos <- variantArmBandData$user_name == 'carlitos'
    variantArmBandData$is_carlitos <- as.numeric(is_carlitos)
    
    is_pedro <- variantArmBandData$user_name == 'pedro'
    variantArmBandData$is_pedro <- as.numeric(is_pedro)
    
    is_jeremy <- variantArmBandData$user_name == 'jeremy'
    variantArmBandData$is_jeremy <- as.numeric(is_jeremy)
    
    is_eurico <- variantArmBandData$user_name == 'eurico'
    variantArmBandData$is_eurico <- as.numeric(is_eurico)
    
    is_charles <- variantArmBandData$user_name == 'charles'
    variantArmBandData$is_charles <- as.numeric(is_charles)
    
    user_name_index <- getColumnIndexByLabel("user_name", variantArmBandData)
    variantArmBandData <- variantArmBandData[,-user_name_index]
    variantArmBandData
}
# This method removes covariates which are irrelevant or disadvantageous to the prediction model
filterOutIrrelevantVariantColumns <- function(variantArmBandData)
{
    relevantColumnsData <- variantArmBandData
    
    # This column is represented by the raw_timestamp_part_1 column
    cvtd_timestamp_index <- getColumnIndexByLabel("cvtd_timestamp", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-cvtd_timestamp_index]
    
    timestamp_1_index <- getColumnIndexByLabel("raw_timestamp_part_1", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-timestamp_1_index]
    
    timestamp_2_index <- getColumnIndexByLabel("raw_timestamp_part_2", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-timestamp_2_index]
    
    user_name_index <- getColumnIndexByLabel("user_name", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-user_name_index]
    
    #num_window_index <- getColumnIndexByLabel("num_window", relevantColumnsData)
    #relevantColumnsData <- relevantColumnsData[,-num_window_index]
    
    # This column is arbitrary data that will introduce noise into our model
    X_index <- getColumnIndexByLabel("X", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-X_index]
    relevantColumnsData
}
# This method removes covariates whose values are at least 80% NA
removeColumnsThatAreMostlyNA <- function(armBandData, na_ratio_threshold = .001)
{
    number_of_columns <- ncol(armBandData)
    column_is_na_list <- c()
    number_of_data_values <- nrow(armBandData)
    for(i in 1:number_of_columns)
    {
        column_of_data <- armBandData[,i]
        na_values_in_column <- is.na(column_of_data)
        number_of_na_values = sum(na_values_in_column == TRUE)
        ratio_of_na_values <- number_of_na_values / number_of_data_values
        column_is_na <- ratio_of_na_values > na_ratio_threshold
        column_is_na_list <- c(column_is_na_list, column_is_na)
    }
    
    nonNaArmBandData = armBandData[,!column_is_na_list]
    nonNaArmBandData
}
# This method filters out covariates which have near zero variance, and as such offer very little information
filterOutNonVariantColumns <- function(armBandData)
{
    noVarianceColumnData <- nearZeroVar(armBandData,saveMetrics=TRUE)
    variantColumnData <- armBandData[,!noVarianceColumnData$nzv]
    variantColumnData
}
# This method returns the integer index of the column whose label is passed in
getColumnIndexByLabel <- function(column_label, dataFrame)
{
    column_label <- grep(column_label, colnames(dataFrame))
    column_label
}

getRowsWithFullData <- function(armBandData, na_ratio_threshold=0.3)
{
    number_of_rows = nrow(armBandData)
    number_of_columns = ncol(armBandData)
    row_is_full_data_list = c()
    
    row_is_full_data_list = apply(armBandData, 1, isRowFullData)
    fullDataFrame = armBandData[row_is_full_data_list,]
    fullDataFrame
}

isRowFullData <- function(row_of_data)
{
    na_ratio_threshold=0.3
    na_values_in_row = is.na(row_of_data)
    number_of_na_values = sum(na_values_in_row == TRUE)
    number_of_columns = ncol(armBandData)
    ratio_of_na_values <- number_of_na_values / number_of_columns
    row_is_full_data <- ratio_of_na_values < na_ratio_threshold
    row_is_full_data
}
