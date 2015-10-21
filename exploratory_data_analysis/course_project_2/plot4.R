source("./read_summary_data.R");
library("dplyr");
library("ggplot2");

coal_combustion_related_sources <- c("Fuel Comb - Comm/Institutional - Coal",
  "Fuel Comb - Electric Generation - Coal",
  "Fuel Comb - Industrial Boilers, ICEs - Coal");

coalCombustionSourceClassificationDataFrame <- filter(sourceClassifications, EI.Sector %in% coal_combustion_related_sources);
scc_values_related_to_coal_combustion_sources <- unique(coalCombustionSourceClassificationDataFrame$SCC);

summaryDataRelatedToCoalCombustionSources <- filter(summaryData, SCC %in% scc_values_related_to_coal_combustion_sources);
summaryDataRelatedToCoalCombustionSources <- transform(summaryDataRelatedToCoalCombustionSources, year = factor(year));
coalCombustionSourcesDataGroupedByYear <- group_by(summaryDataRelatedToCoalCombustionSources, year);
totalCoalRelatedEmissionsGroupedByYear <- summarize(coalCombustionSourcesDataGroupedByYear, total_emissions=sum(Emissions));

totalCoalRelatedEmissionsGroupedByYear$total_emissions <- as.numeric(totalCoalRelatedEmissionsGroupedByYear$total_emissions);

g <- ggplot(totalCoalRelatedEmissionsGroupedByYear, aes(year, total_emissions, group = 1));
g <- g + geom_point(size=4);
g <- g + geom_smooth(method = "lm", size=2, se=FALSE, col="black");
g <- g + labs(title = "PM2.5 Emissions From Coal\nCombustion Sources By Year") +  labs(x = "Year", y = "Total Emissions of PM2.5")
print(g)

dev.copy(png, file="plot4.png", width=480, height=480);
dev.off();
