
library(naturecounts)
library(tidyverse)


lpbo<-read.csv("Data/LPBO2024_Band.csv")

lpbo_sum<-lpbo %>% group_by(Species) %>% summarise(totband = length(Species))


write.csv(lpbo_sum, "Data/sum_fall_band_2024.csv")
