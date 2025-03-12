#.libPaths("/mnt/nfs/mariana/R.script/library")

library(dplyr)
library(tidyr)

#setwd("/home/mariana/mhmrun_CZ/Q")
setwd("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Dalia/DPS3_CZ/")


#make_activation <- function(id){
  
  df <- read.table("DPS3_eobs.dat", skip = 2, col.names = c("date", "Q"))
  #df <-read.table(file = paste0("/Q/discharge_", id, ".dat"), skip = 2, col.names = c("date", "Q"))
  
  df$date <- as.Date(df$date)
  df$year <- as.numeric(format(df$date, format = "%Y"))
  
  q180d <- df %>%
    group_by(year) %>%       # Skupina podle roku
    summarize(Q180 = quantile(Q, probs = 0.5, na.rm = TRUE)) # Mediánový průtok
  
  df <- df %>% left_join(q180d, by = "year")

  #floodplain aktivace
  for (i in 1:nrow(df)){
    if (df$Q[i] >= df$Q180[i]) {
      df$floodplain[i] <- 0.025 * (df$Q[i] * 86400) 
    } else {
      df$floodplain[i] <- 0
    }
  }
  
  #suma za rok
  activation <- df %>%
    group_by(year) %>%
    summarise(
      floodplain = sum(floodplain)
    )
 
  
  #sum of activations per decade
  df <- df %>% 
   mutate(decade = floor(year / 10)*10) 
  
  activation <- df %>%
    group_by(decade) %>%
    summarise(
      activation = sum(real_act)
    )
  
  activation <- activation %>%
    pivot_wider(names_from = decade, values_from = activation)
  
  return(activation)

#}
  

models <- matrix(ncol = 14, nrow = 20)
for (i in 1:nrow(models)){
  c <- make_activation(i)
  models[i,] <- unlist(c)
}


print(models)
real <- matrix(NA, nrow = 1, ncol = 14)
real[, 1:7] <- c(23, 21, 31, 25, 20, 25, 6)

result <- rbind(real, models)

colnames(result) <- seq(from = 1960, to = 2099, by = 10)

rownames(result) <- c("OBSERVATION",
                        "GFDL-ESM2M hist_rcp2p6", "GFDL-ESM2M hist_rcp4p5", "GFDL-ESM2M hist_rcp6p0", "GFDL-ESM2M hist_rcp8p5",
                        "HadGEM2-ES hist_rcp2p6", "HadGEM2-ES hist_rcp4p5", "HadGEM2-ES hist_rcp6p0", "HadGEM2-ES hist_rcp8p5",
                        "IPSL-CM5A-LR hist_rcp2p6", "IPSL-CM5A-LR hist_rcp4p5", "IPSL-CM5A-LR hist_rcp6p0", "IPSL-CM5A-LR hist_rcp8p5",
                        "MIROC-ESM-CHEM hist_rcp2p6", "MIROC-ESM-CHEM hist_rcp4p5", "MIROC-ESM-CHEM hist_rcp6p0", "MIROC-ESM-CHEM hist_rcp8p5",
                        "NorESM1-M hist_rcp2p6", "NorESM1-M hist_rcp4p5", "NorESM1-M hist_rcp6p0", "NorESM1-M hist_rcp8p5")

write.table(result, file = "/home/mariana/R.out/activation.res",row.names = TRUE, col.names = TRUE, quote = FALSE)


write.csv(result, file = "/home/mariana/R.out/activation.csv",row.names = TRUE, col.names = TRUE, quote = FALSE)


  


