.libPaths("/home/mariana/R.script/library")

library(dplyr)
library(tidyr)

setwd("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/Dalia_Git/DPS5_SRB")
make_duration <- function(id){

  Q <- read.csv2("OBS_Novi_Sad.csv", col.names = c("date", "Q", "H", "H_masl"))

  Q$date <- as.Date(Q$date)
  
  df <- data.frame(date = Q$date, Q = Q$Q, H = Q$H, year = year(Q$date))

  #active bypass
  for (i in 1:nrow(df)){
    if (df$H[i] >= 560) {
      df$bypass[i] <- 1
    } else {
      df$bypass[i] <- 0
    }
  }
  
  #bypass activation
  for (i in 1:nrow(df)) {
    if (i == 1 && df$bypass[i] == 1) {
      df$activation[i] <- 1  
    } else if (i == nrow(df) && df$bypass[i] == 1 && df$bypass[i - 1] == 0) {
      df$activation[i] <- 1  
    } else if (df$bypass[i] == 1 && df$bypass[i + 1] != 1 && df$bypass[i - 1] != 1) {
      df$activation[i] <- 1
    } else if (df$bypass[i] == 1 && df$bypass[i + 1] == 1 && df$bypass[i - 1] != 1) {
      df$activation[i] <- 1
    } else {
      df$activation[i] <- 0
    }
  }
  
  #duration of each activation
  count_one <- rle(df$bypass == 1)
  ones <- count_one$lengths[count_one$values]
  
  df$duration <- 0
  counter <- 1
  for (i in 1:nrow(df)) {
    if(df$activation[i] == 1){
      df$duration[i] <- ones[counter]
      counter <- counter + 1
    }
  }
  
  for(i in 1:nrow(df)){
    if(df$duration[i] <= 14 && df$duration[i] > 0) {
      df$good[i] <- 1
    } else {
      df$good[i] <- 0
    }
  }
  
  for(i in 1:nrow(df)){
    if(df$duration[i] > 14) {
      df$bad[i] <- 1
    } else {
      df$bad[i] <- 0
    }
  }
  
  
  #sum of activations per decade
  df <- df %>% 
   mutate(decade = floor(year / 10)*10) 
  
  good <- df %>%
    group_by(decade) %>%
    summarise(
      activation = sum(good)
    )
  
  good <- good %>%
    pivot_wider(names_from = decade, values_from = activation)
  
  
  bad <- df %>%
    group_by(decade) %>%
    summarise(
      activation = sum(bad)
    )
  
  bad <- bad %>%
    pivot_wider(names_from = decade, values_from = activation)
  
  return(good)


}


models <- matrix(ncol = 14, nrow = 20)
for (i in 1:nrow(models)){
  c <- make_duration(i)
  models[i,] <- unlist(c)
}


print(models)

real <- matrix(NA, nrow = 1, ncol = 14)
real[, 1:7] <- c(85, 90, 91, 70, 79, 61, 20)

result <- rbind(real, models)

colnames(result) <- seq(from = 1960, to = 2099, by = 10)

rownames(result) <- c("OBSERVATION",
                        "GFDL-ESM2M hist_rcp2p6", "GFDL-ESM2M hist_rcp4p5", "GFDL-ESM2M hist_rcp6p0", "GFDL-ESM2M hist_rcp8p5",
                        "HadGEM2-ES hist_rcp2p6", "HadGEM2-ES hist_rcp4p5", "HadGEM2-ES hist_rcp6p0", "HadGEM2-ES hist_rcp8p5",
                        "IPSL-CM5A-LR hist_rcp2p6", "IPSL-CM5A-LR hist_rcp4p5", "IPSL-CM5A-LR hist_rcp6p0", "IPSL-CM5A-LR hist_rcp8p5",
                        "MIROC-ESM-CHEM hist_rcp2p6", "MIROC-ESM-CHEM hist_rcp4p5", "MIROC-ESM-CHEM hist_rcp6p0", "MIROC-ESM-CHEM hist_rcp8p5",
                        "NorESM1-M hist_rcp2p6", "NorESM1-M hist_rcp4p5", "NorESM1-M hist_rcp6p0", "NorESM1-M hist_rcp8p5")

write.table(result, file = "/home/mariana/R.out/duration.res",row.names = TRUE, col.names = TRUE, quote = FALSE)


write.csv(result, file = "/home/mariana/R.out/duration.csv", row.names = TRUE, col.names = TRUE, quote = FALSE)


  


