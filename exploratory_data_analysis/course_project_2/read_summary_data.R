pmSummaryFactory <- function()
{
    summaryDataFrame <- NULL;
    sourceClassificationCodes <- NULL;
    
    readInData <- function()
    {
        summaryDataFrame <<- readRDS("summarySCC_PM25.rds");
        sourceClassificationCodes <<- readRDS("Source_Classification_Code.rds");
    }
    
    getSummaryDataFrame <- function()
    {
        summaryDataFrame;
    }
    
    getSourceClassificationCodes <- function()
    {
        sourceClassificationCodes;
    }
    
    list(
        readInData = readInData,
        getSummaryDataFrame = getSummaryDataFrame,
        getSourceClassificationCodes = getSourceClassificationCodes
    )
}

pmSummaryInstance <- pmSummaryFactory();
pmSummaryInstance$readInData();
summaryData <- pmSummaryInstance$getSummaryDataFrame();

