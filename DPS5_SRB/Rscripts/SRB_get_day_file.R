library(data.table)
library(lubridate)

org <- read.csv2("OBS_Novi_Sad.csv")
org$DATE <- as.Date(org$DATE)
appen <- data.frame(date = seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "day"),
                    discharge = NA)


daily <- data.frame(year = year(org$DATE),
                    month = sprintf("%02d", month(org$DATE)),
                    day = sprintf("%02d", day(org$DATE)),
                    hour = rep("00", times = nrow(org)),
                    minute  = rep("00", times = nrow(org)),
                    Q = org$Q.m3.s.)

daily <- daily[daily$year >= 1948, ]
daily$Q[is.na(daily$Q)] <- "-9999.000"

write.table(daily, "4525488.day", col.names = FALSE, quote = FALSE, row.names = FALSE, sep = "\t")

start_lat <- 42
start_lon <- 8

stop_lat <- 48.790911
stop_lon <- 16.437539

step <- 0.001953125

row <- (stop_lat - start_lat) / step
col <- (stop_lon - start_lon) / step
