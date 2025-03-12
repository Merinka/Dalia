library(hydroGOF)
library(dplyr)
library(data.table)
library(ggplot2)
library("ggpubr")

#POST CALIB
br_Q <- read.csv("/Users/mariana/Desktop/Dalia/DPS1_SK/post calib/SK_Discharge_post_calib.csv", row.names = 1)
br_Q$date <- as.Date(br_Q$date)

br_Q <- br_Q %>%
  mutate(year = as.numeric(format(date, format = "%Y"))) %>%
  mutate(decade = floor(year / 10)*10,)

gof_post <- gof(br_Q$model,br_Q$real)

br_Q <- br_Q %>%
  mutate(year = as.numeric(format(date, format = "%Y"))) %>%
  mutate(decade = floor(year / 10)*10,)

br_list <- split(br_Q, br_Q$decade)
index <- nrow(br_list[[1]]) /2
ondex_row <- nrow(br_list[[1]])

#PRE CALIB
br_pre <- read.csv("/Users/mariana/Desktop/Dalia/DPS1_SK/pre calib/SK_Discharge_pre_calib.csv", row.names = 1)

br_pre$date <- as.Date(br_pre$date)

br_pre <- br_pre %>%
  mutate(year = as.numeric(format(date, format = "%Y"))) %>%
  mutate(decade = floor(year / 10)*10,)

gof_pre <- gof(br_pre$model,br_pre$real)

br_pre <- br_pre %>%
  mutate(year = as.numeric(format(date, format = "%Y"))) %>%
  mutate(decade = floor(year / 10)*10,)

br_list_pre <- split(br_pre, br_pre$decade)
index_pre <- nrow(br_list_pre[[1]]) /2
ondex_row_pre <- nrow(br_list_pre[[1]])


#GRAFY
#Průtoky 1960

p60pre <- ggplot(data = br_list_pre[["1960"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "\n\nBefore calibration", subtitle = expression(paste(R^2,"= 0.84, PBIAS = -20.8 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p60post <- ggplot(data = br_list[["1960"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "After calibration", subtitle = expression(paste(R^2,"= 0.86, PBIAS = -11.5 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p60pre

gof_post <- gof(br_Q$model[1:3653],br_Q$real[1:3653])
gof_pre <- gof(br_pre$model[1:3653],br_pre$real[1:3653])


figure60 <- ggarrange(p60pre, p60post,
                         ncol = 1, nrow = 2,
                         common.legend = TRUE)
figure60

#Průtoky 1970
p70pre<- ggplot(data = br_list_pre[["1970"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.5) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.45) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "Before calibration", subtitle = expression(paste(R^2,"= 0.80, PBIAS = -25.9 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p70pre


p70post <- ggplot(data = br_list[["1970"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=12), plot.title = element_text(size = 20), plot.subtitle = element_text(size = 18)) +
  labs(title = "\nAfter calibration", subtitle = expression(paste(R^2,"= 0.84, PBIAS = -15.6 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p70post

gof_post <- gof(br_Q$model[3654:7305],br_Q$real[3654:7305])
gof_pre <- gof(br_pre$model[3654:7305],br_pre$real[3654:7305])


figure70 <- ggarrange(p70pre, p70post,
                      ncol = 1, nrow = 2,
                      common.legend = TRUE)
figure70


#Průtoky 1980
p80pre<- ggplot(data = br_list_pre[["1980"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "Before calibration", subtitle = expression(paste(R^2,"= 0.80, PBIAS = -23.6 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)


p80post <- ggplot(data = br_list[["1980"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=10)) +
  labs(title = "After calibration", subtitle = expression(paste(R^2,"= 0.80, PBIAS = -14.3 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

gof_post <- gof(br_Q$model[7306:10958],br_Q$real[7306:10958])
gof_pre <- gof(br_pre$model[7306:10958],br_pre$real[7306:10958])


figure80 <- ggarrange(p80pre, p80post,
                      ncol = 1, nrow = 2,
                      common.legend = TRUE)
figure80


#Průtoky 1990-2000
p90pre<- ggplot(data = br_list_pre[["1990"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=12), plot.title = element_text(size = 20), plot.subtitle = element_text(size = 18)) +
  labs(title = "\nBefore calibration", subtitle = expression(paste(R^2,"= 0.78, PBIAS = -22.2 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p90pre

p90post <- ggplot(data = br_list[["1990"]], aes(x=date)) +
  geom_line(aes(y = real, color ="OBS",), size = 0.4) +
  geom_line(aes(y = model, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = c(500,1500,2500,3500,4500,5500,6500,7500,8500)) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.text = element_text(size=12), plot.title = element_text(size = 20), plot.subtitle = element_text(size = 18)) +
  labs(title = "\nAfter calibration", subtitle = expression(paste(R^2,"= 0.81, PBIAS = -11.9 %"))) +
  ylab("Q [m3/s]") +
  xlab(NULL)

p90post 

gof_post <- gof(br_Q$model[10959:14610],br_Q$real[10959:14610])
gof_pre <- gof(br_pre$model[10959:14610],br_pre$real[10959:14610])


figure90 <- ggarrange(p90pre, p90post,
                      ncol = 1, nrow = 2,
                      common.legend = TRUE)
figure90

