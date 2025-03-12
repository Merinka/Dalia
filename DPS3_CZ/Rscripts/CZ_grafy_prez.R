library(ggplot2)
library(reshape2)

data <- read.csv("/Users/mariana/Desktop/Dalia/DPS3_CZ/actET_Soutok.csv", check.names = FALSE)

data_melted <- melt(data)
colnames(data_melted) <- c("Model", "Decade", "Actual ")
data_melted$Order <- rep(20:1, 14)

ggplot(data_melted, aes(x = Decade, y = reorder(Model,Order), fill = `Actual Evapotranspiration`)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  geom_text(aes(label = round(`Actual Evapotranspiration`, 1)), color = "black", size = 3) + 
  theme_minimal() +
  labs(fill='Actual \nevapotranspiration\n[mm]\n',
    x = "\n Decade",
    y = "Climate model")


