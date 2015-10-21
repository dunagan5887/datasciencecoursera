source("./read_summary_data.R");
library("dplyr");
library("ggplot2");

baltimore_city_fips <- "24510";
baltimoreCityData <- filter(summaryData, fips == baltimore_city_fips);

baltimoreCityData = transform(baltimoreCityData, year = factor(year));
baltimoreCityData = transform(baltimoreCityData, type = factor(type));

baltimoreCityGroupedByTypeAndYear <- group_by(baltimoreCityData, type, year);
totalEmissionsGroupedByTypeAndYear <- summarize(baltimoreCityGroupedByTypeAndYear, total_emissions=sum(Emissions));

g <- ggplot(totalEmissionsGroupedByTypeAndYear, aes(year, total_emissions, group = 1));
g <- g + geom_point(aes(color = type), size=4);
g <- g + facet_grid(. ~ type);
g <- g + geom_smooth(aes(color = type), method = "lm", size=2, se=FALSE);
g <- g + labs(title = "Total PM2.5 emissions by year and type for Baltimore City") +  labs(x = "Year", y = "Total Emissions of PM2.5")
print(g)

dev.copy(png, file="plot3.png", width=680, height=480);
dev.off();
