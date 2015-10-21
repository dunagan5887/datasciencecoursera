source("./read_summary_data.R");
library("dplyr");
library("ggplot2");

baltimore_city_fips <- "24510";
baltimoreCityData <- filter(summaryData, fips == baltimore_city_fips);
baltimoreCityData <- transform(baltimoreCityData, year = factor(year));

motor_vehicle_sources <- c("Mobile - On-Road Diesel Heavy Duty Vehicles",
                           "Mobile - On-Road Diesel Light Duty Vehicles",
                           "Mobile - On-Road Gasoline Heavy Duty Vehicles",
                           "Mobile - On-Road Gasoline Light Duty Vehicles");

motorVehicleSourcesClassificationDataFrame <- filter(sourceClassifications, EI.Sector %in% motor_vehicle_sources);
scc_values_related_to_motor_vehicle_sources <- unique(motorVehicleSourcesClassificationDataFrame$SCC);

baltimoreCityDataRelatedToMotorVehicleSources <- filter(baltimoreCityData, SCC %in% scc_values_related_to_motor_vehicle_sources);
baltimoreCityMotorVehicleDataByYear <- group_by(baltimoreCityDataRelatedToMotorVehicleSources, year);
baltimoreCityMotorVechileEmissionsByYear <- summarize(baltimoreCityMotorVehicleDataByYear, motor_vehicle_emissions=sum(Emissions));

baltimoreCityMotorVechileEmissionsByYear$motor_vehicle_emissions <- as.numeric(baltimoreCityMotorVechileEmissionsByYear$motor_vehicle_emissions);

g <- ggplot(baltimoreCityMotorVechileEmissionsByYear, aes(year, motor_vehicle_emissions, group = 1));
g <- g + geom_point(size=4);
g <- g + geom_smooth(method = "lm", size=2, se=FALSE, col="black");
g <- g + labs(title = "Baltimore City PM2.5 Emissions From\n Motor Vehicle Sources By Year") +  labs(x = "Year", y = "Motor Vehicle Emissions of PM2.5")
print(g)

dev.copy(png, file="plot5.png", width=480, height=480);
dev.off();
