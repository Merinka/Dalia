library(data.table)
library(lubridate)


org <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/obs data/Qdaily.dat", col.names = c("DATE", "Q"))
org$DATE <- as.Date(org$DATE)
appen <- data.frame(date = seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "day"),
                    discharge = NA)


daily <- data.frame(year = year(org$DATE),
                    month = sprintf("%02d", month(org$DATE)),
                    day = sprintf("%02d", day(org$DATE)),
                    hour = rep("00", times = nrow(org)),
                    minute  = rep("00", times = nrow(org)),
                    Q = round(org$Q,3))

daily <- daily[daily$year >= 1948, ]
daily$Q[is.na(daily$Q)] <- "-9999.000"

write.table(daily, "C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/48349805.day", col.names = FALSE, quote = FALSE, row.names = FALSE, sep = "\t")

