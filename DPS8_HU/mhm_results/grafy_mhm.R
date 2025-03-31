library(ggplot2)
library(data.table)
dta <- read.csv("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/mhm_results/waste.csv", row.names = 1, check.names = FALSE)
g <- mean(unlist(dta[1,]), na.rm = TRUE)
means <- c()
norm <- data.frame()

for(i in 1:nrow(dta)){
  means[i] <- mean(unlist(dta[i,5:6])) 
}

koef <- c()
for(i in 1:length(means)){
  koef[i] <- g/means[i]
}

for(i in 1:nrow(dta)){
  for(j in 1:ncol(dta)){
    norm[i,j] <- dta[i,j] * koef[i]
  }
}

rownames(norm) <- rownames(dta)
colnames(norm) <- colnames(dta)

write.csv2(norm, "C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/Dalia_Git/DPS5_SRB/eco_alarm_norm.csv")

data <- read.csv("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/mhm_results/waste.csv", check.names = FALSE)

data_melted <- melt(data)
colnames(data_melted) <- c("Model", "Decade", "Waste")
data_melted$Order <- rep(20:1, 14)
data_melted$Waste = round(data_melted$Waste/10, 0)

ggplot(data_melted, aes(x = Decade, y = reorder(Model,Order), fill = Waste)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "orange", name = "Days with plastic\nwaste per year\n") +
  geom_text(aes(label = round(Waste, 1)), color = "black", size =4.5) + 
  theme_minimal() +
  theme(axis.text = element_text(size = 12), legend.text = element_text(size=12)) +
  xlab(NULL)+
  ylab("\n")


means <- read.csv2("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/GIT/Dalia/DPS8_HU/mhm_results/waste_mean.csv", row.names = 1, check.names = FALSE)
means_df <- data.frame(decade = year(as.Date(colnames(means), format = "%Y")), mean = unlist(means[21, ]/10), max = unlist(means[22, ]/10), min = unlist(means[23, ]/10))


ggplot(means_df, aes(x = decade)) +
  geom_line(aes(y = mean, colour = "MEAN"), linewidth = 1) +
  geom_line(aes(y = max, colour = "MAX"), linewidth = 1) +
  geom_line(aes(y = min, colour = "MIN"), linewidth = 1) +
  scale_color_manual(values = c("MIN" = "aquamarine3", "MEAN" = "blue", "MAX" = "coral2")) +
  scale_x_continuous(breaks=seq(1960,2090,by=10)) +
  theme_bw() +
  xlab("\nDecade") +
  ylab("Days with plastic waste\n") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(title = "DPS8 - Days with plastic waste per year - model average") 



