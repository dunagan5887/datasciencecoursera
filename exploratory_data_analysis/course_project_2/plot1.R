source("./read_summary_data.R");

totalEmissionsByYear <- tapply(summaryData$Emissions, summaryData$year, sum);
years_in_data <- names(totalEmissionsByYear);
years_in_data <- as.numeric(years_in_data);
totalEmissionsByYear <- as.numeric(totalEmissionsByYear);

dataFrameToPlot <- data.frame(years = years_in_data, emissions = totalEmissionsByYear);

plot(dataFrameToPlot$years, dataFrameToPlot$emissions, pch = 19, main="Total PM2.5 Emissions By Year", ylab="Emissions of PM2.5", xlab="Year");
model <- lm(emissions ~ years, dataFrameToPlot)
abline(model, lwd = 2)

dev.copy(png, file="plot1.png", width=480, height=480);
dev.off();
