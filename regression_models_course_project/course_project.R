library(datasets);
library(ggplot2);

mtcarsRegressionAnalyzerFactory <- function()
{
    automatic_transmission_value <- 0;
    manual_transmission_value <- 1;
    
    automaticCarsDataFrame <- NULL;
    manualCarsDataFrame <- NULL;
    
    mpg_summary <- summary(mtcars$mpg);
    max_mpg_value <- mpg_summary['Max.'];
    min_mpg_value <- mpg_summary['Min.'];
    
    separateAutomaticAndManualTransmissionCars <- function()
    {
        is_manual_logical <- mtcars$am == manual_transmission_value;
        manualCarsDataFrame <<- mtcars[is_manual_logical,];
        
        is_automatic_logical <- mtcars$am == automatic_transmission_value;
        automaticCarsDataFrame <<- mtcars[is_automatic_logical,];
        
        manual_summary <- summarizeMpgByManualTransmission();
        automatic_summary <- summarizeMpgByAutomaticTransmission();
        
    }

    getAutomaticCars <- function()
    {
        automaticCarsDataFrame
    }
    
    getManualCars <- function()
    {
        manualCarsDataFrame
    }
    
    boxplotMpgByTransmissions <- function()
    {
        par(mfrow=c(1,1), oma=c(0,2,2,0))
        boxplot(mpg ~ am, data = mtcars, col=c("Blue", "Red"), range=0, boxwex = 0.4,
                ylab="MPG", main = "MPG by Transmission Type", xlab="Transmission Type",
                names=c("Automatic", "Manual"))
        legend("topleft", col=c("Blue", "Red"), legend = c("Automatic", "Manual"),
               lty=c(1,1,1), lwd=c(2.5,2.5,2.5))
        #dev.copy(png, file="mpg_by_transmission_boxplot.png")
        #dev.off();
    }
    
    historgramMpgByTransmissions <- function()
    {
        par(mfrow=c(2,1), oma=c(0,2,2,0))
        with(mtcars, {
            hist(automaticCarsDataFrame$mpg, xlim=c((min_mpg_value - 1), (max_mpg_value + 1)), ylim=c(0,3), 
                 breaks=(min_mpg_value - 1):(max_mpg_value + 1),
                 col="Black", border="White", xlab="MPG", main="Automatic Transmission",
                 ylab = "Number Of Cars")
            hist(manualCarsDataFrame$mpg, xlim=c((min_mpg_value - 1), (max_mpg_value + 1)), ylim=c(0,3), 
                 breaks=(min_mpg_value - 1):(max_mpg_value + 1),
                 col="Black", border="White", xlab="MPG", main="Manual Transmission",
                 ylab = "Number Of Cars")
            mtext("MPG by Transmission Types", outer=TRUE)
        })
        
        #dev.copy(png, file="mpg_by_transmission_histograms.png")
        #dev.off();
    }
    
    executeTIntervalTest <- function()
    {
        separateAutomaticAndManualTransmissionCars();
        automaticCarsDataFrame <- mtcarsRegressionAnalyzerInstance$getAutomaticCars();
        manualCarsDataFrame <- mtcarsRegressionAnalyzerInstance$getManualCars();
        result <- t.test(manualCarsDataFrame$mpg, automaticCarsDataFrame$mpg, paired=FALSE,
                         var.equal=FALSE)
        result
    }
    
    summarizeMpgByManualTransmission <- function()
    {
        summary(manualCarsDataFrame$mpg);
    }
    
    summarizeMpgByAutomaticTransmission <- function()
    {
        summary(automaticCarsDataFrame$mpg);
    }
    
    plotLinearModelBetweenMpgAndTransmissionUsingBase <- function()
    {
        linearModel <- lm(mtcars$mpg ~ mtcars$am);
        mean_automatic_mpg <- mean(automaticCarsDataFrame$mpg)
        mean_manual_mpg <- mean(manualCarsDataFrame$mpg)
        mean_mpg <- c(mean_automatic_mpg, mean_manual_mpg)
        tranmission_types <- c(0,1)
        
        par(mfrow=c(1,1), oma=c(0,2,2,0))
        with(mtcars, {
            plot(automaticCarsDataFrame$am, automaticCarsDataFrame$mpg, pch=17,
                 ylim=c((min_mpg_value - 1), (max_mpg_value + 1)), col="Blue",
                 xlim=c(-.5,1.5), ylab="MPG", xlab="Transmission Type", 
                 main="Linear Relationship between\nMPG and Transmission Type")
            points(manualCarsDataFrame$am, manualCarsDataFrame$mpg, pch=17, col="Red")
            abline(linearModel, lwd=2)
            abline(h=mean_automatic_mpg, lwd=1, col="Blue")
            abline(h=mean_manual_mpg, lwd=1, col="Red")
            legend("topleft", col=c("Black", "Blue", "Red"),
                   legend=c("Linear Relationship Fit", "Automatic Empirical Mean", "Manual Empirical Mean"),
                   lty=c(1,1,1), lwd=c(2.5,2.5,2.5), cex=.85, bty="n")
            legend("bottomright", col=c("Blue", "Red"), 
                   legend=c("Automatic MPG Values", "Manual MPG Values"), pch=c(17,17), 
                   cex=.75, bty="n")
        })
        
        #dev.copy(png, file="linear_regression_plot_using_base.png")
        #dev.off();
    }
    
    plotLinearModelBetweenMpgAndTransmissionUsingGgplot <- function()
    {
        linearModel <- lm(mtcars$mpg ~ mtcars$am);
        mean_automatic_mpg <- mean(automaticCarsDataFrame$mpg)
        mean_manual_mpg <- mean(manualCarsDataFrame$mpg)
        mean_mpg <- c(mean_automatic_mpg, mean_manual_mpg)
        tranmission_types <- c(0,1)
        
        g <- ggplot(mtcars, aes(am, mpg))
        g <- g + geom_point()
        g <- g + geom_smooth(method = "lm")
        g <- g + labs(title = "MtCars MPG by Transmission Type") + labs(x = "Is Manual Transmission Type", y = "MPG")
        print(g)
        
        dev.copy(png, file="linear_regression_plot_using_ggplot.png")
        dev.off();
    }
    
    plotResidualsBetweenMpgAndTransmission <- function()
    {
        linearModel <- lm(mtcars$mpg ~ mtcars$am);
        residuals <- resid(linearModel);
        mtcars$residuals <- residuals;
        
        g <- ggplot(mtcars, aes(am, residuals))
        g <- g + geom_point(aes(color = am), size = 2);
        g <- g + geom_smooth(size = 3, method = "lm", se = TRUE)
        g <- g + coord_cartesian(xlim = c(-.25, 1.25));
        print(g)
    }
    
    list (separateAutomaticAndManualTransmissionCars = separateAutomaticAndManualTransmissionCars,
          getAutomaticCars = getAutomaticCars,
          getManualCars = getManualCars,
          boxplotMpgByTransmissions = boxplotMpgByTransmissions,
          summarizeMpgByManualTransmission = summarizeMpgByManualTransmission,
          summarizeMpgByAutomaticTransmission = summarizeMpgByAutomaticTransmission,
          historgramMpgByTransmissions = historgramMpgByTransmissions,
          plotLinearModelBetweenMpgAndTransmissionUsingBase = plotLinearModelBetweenMpgAndTransmissionUsingBase,
          plotLinearModelBetweenMpgAndTransmissionUsingGgplot = plotLinearModelBetweenMpgAndTransmissionUsingGgplot,
          plotResidualsBetweenMpgAndTransmission = plotResidualsBetweenMpgAndTransmission)
}

createBoxPlot <- function()
{
    mtcarsRegressionAnalyzerInstance <- mtcarsRegressionAnalyzerFactory();
    mtcarsRegressionAnalyzerInstance$separateAutomaticAndManualTransmissionCars();
    mtcarsRegressionAnalyzerInstance$boxplotMpgByTransmissions();
    mtcarsRegressionAnalyzerInstance$historgramMpgByTransmissions();
}

createHistogramPlot <- function()
{
    mtcarsRegressionAnalyzerInstance <- mtcarsRegressionAnalyzerFactory();
    mtcarsRegressionAnalyzerInstance$separateAutomaticAndManualTransmissionCars();
    mtcarsRegressionAnalyzerInstance$historgramMpgByTransmissions();
}

testTInterval <- function()
{
    mtcarsRegressionAnalyzerInstance <- mtcarsRegressionAnalyzerFactory();
    mtcarsRegressionAnalyzerInstance$separateAutomaticAndManualTransmissionCars();
    automaticCarsDataFrame <- mtcarsRegressionAnalyzerInstance$getAutomaticCars();
    manualCarsDataFrame <- mtcarsRegressionAnalyzerInstance$getManualCars();
    
    result <- t.test(manualCarsDataFrame$mpg, automaticCarsDataFrame$mpg, paired=FALSE,
                var.equal=FALSE)
    
    result
}

testLinearModelPlot <- function()
{
    mtcarsRegressionAnalyzerInstance <- mtcarsRegressionAnalyzerFactory();
    mtcarsRegressionAnalyzerInstance$separateAutomaticAndManualTransmissionCars();
    mtcarsRegressionAnalyzerInstance$plotLinearModelBetweenMpgAndTransmissionUsingGgplot();
    mtcarsRegressionAnalyzerInstance$plotLinearModelBetweenMpgAndTransmissionUsingBase();
}

plotResiduals <- function()
{
    mtcarsRegressionAnalyzerInstance <- mtcarsRegressionAnalyzerFactory();
    mtcarsRegressionAnalyzerInstance$plotResidualsBetweenMpgAndTransmission();
}

#createBoxPlot()
#createHistogramPlot()
#test_interval <- testTInterval()
#testLinearModelPlot()