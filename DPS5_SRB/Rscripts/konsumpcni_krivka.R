library(ggplot2)
library(dplyr)
library(data.table)

setwd("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/Dalia_Git/DPS5_SRB")
data <- read.csv2("OBS_Novi_Sad.csv", col.names = c("date", "Q", "H", "Hndm"))

data <- data[data$H >= 1,]

plot(data$Q, data$H, main = "Rating curve Novi Sad", xlab = "Q [m3/s]", ylab = "H [cm]", pch = 19, cex = 0.5, col = "snow4")

fit1 <- lm(H ~ poly(Q, 2, raw = TRUE), data = data)

Q_seq <- seq(min(data$Q), max(data$Q), length.out = 100)

H_pred <- predict(fit1, newdata = data.frame(Q = Q_seq), interval = "prediction")
H_pred[H_pred < 0] <- 1

lines(Q_seq, H_pred[,"fit"], col = "blue", lwd = 3)
lines(Q_seq, H_pred[, "lwr"], col = "red", lwd = 2)
lines(Q_seq, H_pred[, "upr"], col = "red", lwd = 2)





