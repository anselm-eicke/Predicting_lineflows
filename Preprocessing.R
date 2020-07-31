library(tidyr)
  
# preprocessline data
preprocess_line_data

# -------------
# preprocessing of covariates

# generation forecast data
generation_forecast = read.csv("Data/Raw data/Forecasted_generation_short.csv")

names(generation_forecast)[names(generation_forecast) == "Gesamt.MWh."] <- "Total_Generation"
names(generation_forecast)[names(generation_forecast) == "Wind.Offshore.MWh."] <- "Offshore"
names(generation_forecast)[names(generation_forecast) == "Wind.Onshore.MWh."] <- "Onshore"
names(generation_forecast)[names(generation_forecast) == "Photovoltaik.MWh."] <- "PV"
names(generation_forecast)[names(generation_forecast) == "Sonstige.MWh."] <- "Other"

generation_forecast$Datetime <- as.POSIXct(generation_forecast$Datetime, format = '%d/%m/%Y %H:%M')

# exchange data
# problem for data since beginning of 2018!
commercial_exchanges <- read.csv("Data/Raw data/Scheduled_commercial_exchanges.csv", fileEncoding="UTF-8-BOM")
commercial_exchanges = unite(commercial_exchanges, Datetime, Date:Time.of.day, sep=' ')
commercial_exchanges$Datetime <- as.POSIXct(commercial_exchanges$Datetime, format = '%d.%m.%Y %H:%M')
commercial_exchanges <-  na.omit(commercial_exchanges)
# price data
# still missing

# merge 
covariates = merge(generation_forecast, commercial_exchanges, by="Datetime")

write.csv(covariates, "Data/Covariates.csv")
