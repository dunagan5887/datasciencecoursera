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
                ylab="MPG", main = "Figure 1\nMPG by Transmission Type", xlab="Transmission Type",
                names=c("Automatic", "Manual"))
        legend("topleft", col=c("Blue", "Red"), legend = c("Automatic", "Manual"),
               lty=c(1,1,1), lwd=c(2.5,2.5,2.5))
    }
    
    summarizeMpgByManualTransmission <- function()
    {
        summary(manualCarsDataFrame$mpg);
    }
    
    summarizeMpgByAutomaticTransmission <- function()
    {
        summary(automaticCarsDataFrame$mpg);
    }

    plotResidualsBetweenMpgAndTransmission <- function()
    {
        amAgainstWeightCylinderCarbs <- lm(am ~ wt + cyl + carb, data = mtcars);
        eAmAgainstWeightCylinderCarbs <- resid(amAgainstWeightCylinderCarbs);
        mpgAgainstWeightCylinderCarbs <- lm(mpg ~ wt + cyl + carb, data = mtcars);
        eMpgAgainstWeightCylinderCarbs <- resid(mpgAgainstWeightCylinderCarbs);
        
        g <- ggplot(mtcars, aes(x = eAmAgainstWeightCylinderCarbs, y = eMpgAgainstWeightCylinderCarbs))
        g <- g + geom_point(aes(color = am), size = 2);
        g <- g + geom_smooth(size = 3, method = "lm", se = TRUE);
        g <- g + labs(title="Residuals Plot To Show relationship\nbetween MPG and Transmission Type");
        g <- g + labs(x="Residuals of Transmission Type\nagainst Weight, Cylinders and Carburetors");
        g <- g + labs(y="Residuals of MPG\nagainst Weight, Cylinders and Carburetors");
        print(g)
    }
    
    getModelResidualsOfMpgAgainstAm <- function()
    {
        amAgainstWeightCylinderCarbs <- lm(am ~ wt + cyl + carb, data = mtcars);
        eAmAgainstWeightCylinderCarbs <- resid(amAgainstWeightCylinderCarbs);
        mpgAgainstWeightCylinderCarbs <- lm(mpg ~ wt + cyl + carb, data = mtcars);
        eMpgAgainstWeightCylinderCarbs <- resid(mpgAgainstWeightCylinderCarbs);
        
        residualMpgAgainstResidualAm <- lm(eMpgAgainstWeightCylinderCarbs ~ eAmAgainstWeightCylinderCarbs);
        residualMpgAgainstResidualAm
    }
    
    list (separateAutomaticAndManualTransmissionCars = separateAutomaticAndManualTransmissionCars,
          getAutomaticCars = getAutomaticCars,
          getManualCars = getManualCars,
          boxplotMpgByTransmissions = boxplotMpgByTransmissions,
          summarizeMpgByManualTransmission = summarizeMpgByManualTransmission,
          summarizeMpgByAutomaticTransmission = summarizeMpgByAutomaticTransmission,
          plotResidualsBetweenMpgAndTransmission = plotResidualsBetweenMpgAndTransmission,
          getModelResidualsOfMpgAgainstAm = getModelResidualsOfMpgAgainstAm)
}

createBoxPlot <- function()
{
    mtcarsRegressionAnalyzerInstance <- mtcarsRegressionAnalyzerFactory();
    mtcarsRegressionAnalyzerInstance$separateAutomaticAndManualTransmissionCars();
    mtcarsRegressionAnalyzerInstance$boxplotMpgByTransmissions();
    mtcarsRegressionAnalyzerInstance$historgramMpgByTransmissions();
}


plotResiduals <- function()
{
    mtcarsRegressionAnalyzerInstance <- mtcarsRegressionAnalyzerFactory();
    mtcarsRegressionAnalyzerInstance$plotResidualsBetweenMpgAndTransmission();
}
