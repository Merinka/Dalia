library(ggplot2)
library(reshape2)
library(dplyr)

data <- read.csv2("/Users/mariana/Library/CloudStorage/OneDrive-CZUvPraze/Dalia/DPS1_SK/activation.csv", header = TRUE)

avg <- as.data.frame(colMeans(data[c(3:6,11:22), -1]))
avg2 <- as.data.frame(colMeans(data[-1:-2, -1]))

average <- data.frame(decade = row.names(avg),
                      activation = avg[,1],
                      obs = c(rep(NA, 4), 24,20,rep(NA,8)))
                    
ggplot(average, aes(x = decade)) +
  geom_point(aes(y = obs, colour = "OBS"), size = 3) +
  geom_point(aes(y=activation, colour = "MODEL"), size = 3) +
  scale_color_manual(values = c("OBS" = "aquamarine3", "MODEL" = "coral2")) +
  theme_bw() +
  scale_y_continuous(breaks = c(15,20,25,30,35,40)) +
  theme(legend.title = element_blank()) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "bottom") +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(legend.text = element_text(size=14), plot.title = element_text(size = 16), plot.subtitle = element_text(size = 13)) +
  theme(axis.text = element_text(size = 13)) + 
  labs(title = "DPS1 Floodplain activation - model average") +
  ylab(NULL) +
  xlab(NULL)

g <- mean(unlist(data[1,2:15]), na.rm = TRUE)
means <- c()
norm <- data.frame()
dta <- data[-1,-1]

for(i in 1:nrow(dta)){
  means[i] <- mean(unlist(dta[i,1:6])) 
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

write.csv2(norm, "norm.csv")


data_melted <- melt(data)
colnames(data_melted) <- c("Model", "Decade", "Activation")
data_melted$Order <- rep(22:1, 14)

ggplot(data_melted, aes(x = Decade, y = reorder(Model,Order), fill = Activation)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = round(Activation, 1)), color = "black", size =5) + 
  theme_minimal() +
  theme(axis.text = element_text(size = 12), legend.text = element_text(size=12)) +
  xlab(NULL)+
  ylab("\n")


