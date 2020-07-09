library(readxl)

Scheduled_commercial_exchanges <- read_excel("Data/Raw data/Scheduled_commercial_exchanges.xlsx")

Forecasted_generation <- read_excel("Data/Raw data/Forecasted_generation.xlsx")

data_time = within(Forecasted_generation[1, ], { timestamp=format(as.POSIXct(paste(Datum, Uhrzeit)), "%D.%M.%Y %H:%M") })

within(Scheduled_commercial_exchanges, { timestamp=format(as.POSIXct(paste(Date, Time of Day)), "%D %M %Y %H:%M") })