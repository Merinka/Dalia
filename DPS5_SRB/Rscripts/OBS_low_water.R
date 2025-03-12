.libPaths("/home/mariana/R.script/library")

library(data.table)
library(dplyr)
library(tidyr)

setwd("C:/Users/hajkovamariana/OneDrive - CZU v Praze/Plocha/Dalia_Git/DPS5_SRB")

make_stage <- function(id){
  
  #Průtok
  
  Q <- read.csv2("OBS_Novi_Sad.csv", col.names = c("date", "Q", "H", "H_masl"))
  
  Q$date <- as.Date(Q$date)
  
  dta <- data.frame(date = Q$date, Q = Q$Q, H = Q$H, year = year(Q$date))
  
  for (i in 1:nrow(dta)) {
    if (dta$H[i] < 110) {
      dta$alarm[i] <- 1
    } else {
      dta$alarm[i] <- 0
    }
  }
  
  dta <- dta%>% 
    mutate(decade = floor(year / 10)*10) 
  
  alarm_year <- dta %>%
    group_by(year) %>%
    summarise(
      alarm = sum(alarm)
    )
  
  alarm_dec <- dta %>%
    group_by(decade) %>%
    summarise(
      alarm = sum(alarm)
    )
  
  alarm_year <- alarm_year %>%
    pivot_wider(names_from = year, values_from = alarm)
  
  alarm_dec <- alarm_dec %>%
    pivot_wider(names_from = decade, values_from = alarm)
  
  return(alarm_dec)
}

models <- matrix(ncol = 14, nrow = 20)
for (i in 1:nrow(models)){
  c <- make_stage(i)
  models[i,] <- unlist(c)
}
real <- matrix(NA, nrow = 1, ncol = 14)
real[, 5:6] <- c(277,586)

result <- rbind(real, models)

colnames(result) <- seq(from = 1960, to = 2099, by = 10)

rownames(result) <- c("OBSERVATION",
                      "GFDL-ESM2M hist_rcp2p6", "GFDL-ESM2M hist_rcp4p5", "GFDL-ESM2M hist_rcp6p0", "GFDL-ESM2M hist_rcp8p5",
                      "HadGEM2-ES hist_rcp2p6", "HadGEM2-ES hist_rcp4p5", "HadGEM2-ES hist_rcp6p0", "HadGEM2-ES hist_rcp8p5",
                      "IPSL-CM5A-LR hist_rcp2p6", "IPSL-CM5A-LR hist_rcp4p5", "IPSL-CM5A-LR hist_rcp6p0", "IPSL-CM5A-LR hist_rcp8p5",
                      "MIROC-ESM-CHEM hist_rcp2p6", "MIROC-ESM-CHEM hist_rcp4p5", "MIROC-ESM-CHEM hist_rcp6p0", "MIROC-ESM-CHEM hist_rcp8p5",
                      "NorESM1-M hist_rcp2p6", "NorESM1-M hist_rcp4p5", "NorESM1-M hist_rcp6p0", "NorESM1-M hist_rcp8p5")

write.table(result, file = "alarm_eco.res",row.names = TRUE, col.names = TRUE, quote = FALSE)


write.csv(result, file = "alarm_eco.csv",row.names = TRUE, col.names = TRUE, quote = FALSE)





    
    
    


