library("downloader");

pmSummaryFactory <- function()
{
    remote_file_location <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip";
    zipped_data_file_name <- 'NEI_data.zip';
    data_has_been_downloaded <- FALSE;
    
    summary_data_filename <- "summarySCC_PM25.rds";
    summaryDataFrame <- NULL;
    source_classification_codes_filename <- "Source_Classification_Code.rds";
    sourceClassificationCodes <- NULL;
    
    readInData <- function()
    {
        if (!data_has_been_downloaded)
        {
            download(remote_file_location, dest=zipped_data_file_name, mode="wb");
            unzip (zipped_data_file_name, exdir = "./")
            data_has_been_downloaded <<- TRUE;
        }
        
        if (is.null(summaryDataFrame))
        {
            summaryDataFrame <<- readRDS(summary_data_filename);
        }
        
        if(is.null(sourceClassificationCodes))
        {
            sourceClassificationCodes <<- readRDS(source_classification_codes_filename);
        }
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

if (!exists("pmSummaryInstance"))
{
    pmSummaryInstance <- pmSummaryFactory();
}

pmSummaryInstance$readInData();
summaryData <- pmSummaryInstance$getSummaryDataFrame();
sourceClassifications <- pmSummaryInstance$getSourceClassificationCodes();
