library(hydroGOF)
library(dplyr)
library(data.table)
library(ggplot2)
library("ggpubr")

#PRE CALIB
pre_Q <- read.csv("/Users/mariana/Desktop/Dalia/DPS3_CZ/pre calib/CZ_Discharge_pre_calib.csv", row.names = 1)
pre_Q$date <- as.Date(pre_Q$date)

#POST CALIB
post_Q <- read.csv("/Users/mariana/Desktop/Dalia/DPS3_CZ/post calib/CZ_Discharge_post_calib.csv", row.names = 1)
post_Q$date <- as.Date(post_Q$date)

post_Q <- post_Q %>%
  filter(year(date) < 2001)


pre <- ggplot(data = pre_Q, aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "\n\nBefore calibration", subtitle = expression(paste(R^2,"= 0.51, PBIAS = -42.0 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

pre
gof_pre <- gof(pre_Q$model,pre_Q$real)


post <- ggplot(data = post_Q, aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "\n\nAfter calibration", subtitle = expression(paste(R^2,"= 0.69, PBIAS = -0.6 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

post

gof_post <- gof(post_Q$model,post_Q$real)


