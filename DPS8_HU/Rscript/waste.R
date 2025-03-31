library(data.table)
library(dplyr)
library(ggplot2)
obs <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/obs data/Qdaily.dat", col.names = c("date", "Q"))
obs <- obs[obs$date >= "2019" & obs$date < "2023",]
waste <- read.csv2("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/obs data/waste.csv", dec = ",")

waste$date <- as.Date(waste$date)
obs$date <- as.Date(obs$date)

obs <- left_join(obs, waste, by = "date")
obs <- obs[,-3]
obs[obs == -9999] <- NA

ggplot(obs, aes(x = date)) +
  geom_line(aes(y = Q, color = "Q")) + 
  geom_bar(aes(y = waste, fill = "waste"), stat = "identity", alpha = 0.5, color = "blue")

obs$dq_dt <- NA

for (i in 2:nrow(obs)){
  obs$dq_dt[i] <- (obs$Q[i] - obs$Q[i-1]) 
}

ggplot(obs, aes(x = date)) +
  geom_line(aes(y = dq_dt, color = "dq_dt")) + 
  geom_bar(aes(y = waste, fill = "waste"), stat = "identity", alpha = 0.5, color = "blue") 
  

obs_lim <- obs[obs$Q >= 150,]

ggplot(obs_lim, aes(x = date)) +
  geom_line(aes(y = Q, color = "Q")) + 
  geom_bar(aes(y = waste, fill = "waste"), stat = "identity", alpha = 0.5, color = "blue")
