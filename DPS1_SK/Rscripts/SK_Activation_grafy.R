library(ggplot2)
library(reshape2)

data <- read.csv("/Users/mariana/Desktop/Dalia/DPS1_SK/activation.csv", check.names = FALSE)

data_melted <- melt(data)
colnames(data_melted) <- c("Model", "Decade", "Activation")
data_melted$Order <- rep(22:1, 14)

ggplot(data_melted, aes(x = Decade, y = reorder(Model,Order), fill = Activation)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = round(Activation, 1)), color = "black", size = 3) + 
  theme_minimal() +
  labs(
        x = "\n Decade",
       y = "Climate model")


