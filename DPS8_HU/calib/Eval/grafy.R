library(ggplot2)
library(hydroGOF)

obs <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/calibrace/Eval/HU_obs_daily.dat", col.names = c("date", "Q"))
sim <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/calibrace/Eval/HU_model.dat", col.names = c("date", "Q"))
obs <- obs[obs$date >= "2016" & obs$date < "2022",]

Fel <- data.frame(date = obs$date, OBS = obs$Q, SIM = sim$Q)
Fel[Fel == -9999] <- NA

write.table(Fel ,"C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/calibrace/Eval/Fel_Q_postcalib.dat", col.names = TRUE, quote = FALSE, row.names = FALSE)


Fel <- read.table("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/calibrace/Eval/Fel_Q_postcalib.dat", header = TRUE)

Fel$date <- as.Date(Fel$date)

gof(Fel$SIM, Fel$OBS)

ggplot(Fel, aes(x = date)) +
  geom_line(aes(y = OBS, colour = "OBS"), size = 0.4) +
  geom_line(aes(y = SIM, colour = "SIM"), size = 0.4) +
  scale_color_manual(values = c("aquamarine3", "coral2")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  labs(title = "Felsőberecki - after calibration", subtitle = "MAE = 71 m3/s, PBIAS = -48 %") +
  ylab("Q [m3/s]\n") +
  xlab(NULL)

