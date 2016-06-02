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
    
    num_window_index <- getColumnIndexByLabel("num_window", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-num_window_index]
    
    user_name_index <- getColumnIndexByLabel("user_name", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-user_name_index]
    
    # This column is arbitrary data that will introduce noise into our model
    X_index <- getColumnIndexByLabel("X", relevantColumnsData)
    relevantColumnsData <- relevantColumnsData[,-X_index]
    relevantColumnsData
}
# This method removes covariates whose values are at least .1% NA
removeColumnsThatHaveNAValues <- function(armBandData, na_ratio_threshold = .001)
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
