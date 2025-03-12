library(data.table)
library(lubridate)
setwd("/Users/mariana/Desktop/Dalia/DPS1_SK")
org <- read.csv("Bratislava_real_full.csv")
org$date <- as.Date(org$date, format = "%Y.%m.%d")
appen <- data.frame(date = seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "day"),
                    discharge = NA)

daily <- data.frame(year = c(year(org$date),year(appen$date)),
                    month = c(sprintf("%02d", month(org$date)), sprintf("%02d", month(appen$date))),
                    day = c(sprintf("%02d", day(org$date)), sprintf("%02d", day(appen$date))),
                    hour = rep("00", times = nrow(org) + nrow(appen)),
                    minute  = rep("00", times = nrow(org) + nrow(appen)),
                    Q = c(org$discharge, appen$discharge))

daily <- daily[daily$year >= 1948, ]
daily$Q[is.na(daily$Q)] <- "-9999.000"

write.table(daily, "6142200.day", col.names = FALSE, quote = FALSE, row.names = FALSE, sep = "\t")

start_lat <- 42
start_lon <- 8

stop_lat <- 48.790911
stop_lon <- 16.437539

step <- 0.001953125

row <- (stop_lat - start_lat) / step
col <- (stop_lon - start_lon) / step
