library(hydroGOF)
library(dplyr)
library(data.table)
library(ggplot2)
library("ggpubr")


Q <- read.table("/Users/mariana/Desktop/Dalia/DPS2_DE/dps2_Q.csv", header = TRUE)

Q$date <- as.Date(Q$date)
Q <- Q %>%
  mutate(year = as.numeric(format(date, format = "%Y"))) %>%
  mutate(decade = floor(year / 10)*10,)

Q_list <- split(Q, Q$decade)


p60 <- ggplot(data = Q_list[["1960"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  #scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "\n\nBefore calibration", subtitle = expression(paste(R^2,"= 0.84, PBIAS = -20.8 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p60

p70 <- ggplot(data = Q_list[["1970"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  #scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "\n\nBefore calibration", subtitle = expression(paste(R^2,"= 0.84, PBIAS = -20.8 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p70

p80 <- ggplot(data = Q_list[["1980"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  #scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "\n\nBefore calibration", subtitle = expression(paste(R^2,"= 0.84, PBIAS = -20.8 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p80

p90 <- ggplot(data = Q_list[["1990"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  #scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "\n\nBefore calibration", subtitle = expression(paste(R^2,"= 0.89, PBIAS = -11.5 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p90

gof(Q_list[["1990"]][["model"]], Q_list[["1990"]][["real"]])




