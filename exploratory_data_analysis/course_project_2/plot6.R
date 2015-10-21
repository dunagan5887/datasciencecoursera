source("./read_summary_data.R");
library("dplyr");
library("ggplot2");

baltimore_city_fips <- "24510";
los_angeles_county_fips = "06037";
fips_in_question <- c(baltimore_city_fips, los_angeles_county_fips);

countySpecificData <- filter(summaryData, fips %in% fips_in_question);
countySpecificData <- transform(countySpecificData, year = factor(year));

motor_vehicle_sources <- c("Mobile - On-Road Diesel Heavy Duty Vehicles",
                           "Mobile - On-Road Diesel Light Duty Vehicles",
                           "Mobile - On-Road Gasoline Heavy Duty Vehicles",
                           "Mobile - On-Road Gasoline Light Duty Vehicles");

motorVehicleSourcesClassificationDataFrame <- filter(sourceClassifications, EI.Sector %in% motor_vehicle_sources);
scc_values_related_to_motor_vehicle_sources <- unique(motorVehicleSourcesClassificationDataFrame$SCC);

dataRelatedToMotorVehicleSources <- filter(countySpecificData, SCC %in% scc_values_related_to_motor_vehicle_sources);
dataRelatedToMotorVehiclesByYear <- group_by(dataRelatedToMotorVehicleSources, fips, year);
groupedAndSummarizedDataRelatedToMotorVehicles <- summarize(dataRelatedToMotorVehiclesByYear, motor_vehicle_emissions=sum(Emissions));

groupedAndSummarizedDataRelatedToMotorVehicles$motor_vehicle_emissions <- as.numeric(groupedAndSummarizedDataRelatedToMotorVehicles$motor_vehicle_emissions);

g <- ggplot(groupedAndSummarizedDataRelatedToMotorVehicles, aes(year, motor_vehicle_emissions, group = 1));
g <- g + geom_point(size=4, aes(color=fips));
g <- g + facet_grid(. ~ fips);
g <- g + geom_smooth(method = "lm", size=2, se=FALSE, aes(color=fips));
g <- g + labs(title = "Baltimore City (24510) vs\nLos Angeles County (06037)\nPM2.5 Emissions From Motor\nVehicle Sources By Year") +  labs(x = "Year", y = "Motor Vehicle Emissions of PM2.5")
print(g)

dev.copy(png, file="plot6.png", width=600, height=600);
dev.off();

