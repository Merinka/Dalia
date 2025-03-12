library(ggplot2)
library(hydroGOF)

#Novi Sad
Novi_sad <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/Dalia_Git/DPS5_SRB/SRB_Novi_sad_Q.dat", header = TRUE)

Novi_sad$date <- as.Date(Novi_sad$date)

gof(Novi_sad$SIM, Novi_sad$OBS)

ggplot(Novi_sad, aes(x = date)) +
  geom_line(aes(y = OBS, colour = "OBS"), size = 0.4) +
  geom_line(aes(y = SIM, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  labs(title = "Novi Sad - after calibration", subtitle = "MAE = 432 m3/s, PBIAS = -10.8 %") +
  ylab("Q [m3/s]\n") +
  xlab(NULL)

#Backa Palanka
Backa <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/Dalia_Git/DPS5_SRB/SRB_Backa_Palanka_Q.dat", header = TRUE)

Backa$date <- as.Date(Backa$date)
gof(Backa$SIM, Backa$OBS)

ggplot(Backa, aes(x = date)) +
  geom_line(aes(y = OBS, colour = "OBS"), size = 0.4) +
  geom_line(aes(y = SIM, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  labs(title = "Backa Palanka", subtitle = "MAE = 476 m3/s, PBIAS = -14.7 %") +
  ylab("Q [m3/s]\n") +
  xlab(NULL)

#Novi Sad - pred kalibraci
Novi_sad_pre <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/Dalia_Git/DPS5_SRB/Before calib/Novi_sad_pre_calib.dat", skip = 2, col.names = c("date", "SIM"))

Novi_sad_pre$date <- as.Date(Novi_sad_pre$date)
Novi_sad_pre$OBS <- Novi_sad$OBS

gof(Novi_sad_pre$SIM, Novi_sad_pre$OBS)

ggplot(Novi_sad_pre, aes(x = date)) +
  geom_line(aes(y = OBS, colour = "OBS"), size = 0.4) +
  geom_line(aes(y = SIM, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  labs(title = "Novi Sad - before calibration", subtitle = "MAE = 680 m3/s, PBIAS = -25.6 %") +
  ylab("Q [m3/s]\n") +
  xlab(NULL)
