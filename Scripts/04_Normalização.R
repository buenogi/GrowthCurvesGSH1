# Normalização

library(tidyverse)
library(ggplot2)

#Loading data

DataGC <- read.csv(file = "Data/Processed/DataGC.csv")

abs_norm <- DataGC%>%
  group_by(pop,experiment, conc)%>%
  summarise(abs_norm = mean(abs))
abs_norm$n_base <-500000 


DataGC_N <- left_join(DataGC, abs_norm, by = c("conc", "experiment", "pop"))

for(i in 1:nrow(DataGC_N)){
  DataGC_N$n_parasitos[i] <-  DataGC_N$abs[i]*DataGC_N$n_base[i]/DataGC_N$abs_norm[i]
}

write.csv(DataGC_N, file = "Data/Processed/DataGC_N.csv")

DataGC_F <-  DataGC_N

# Forçando dia zero com a mesmo nº de parasitos

for(i in 1:nrow(DataGC_F)){
  if(DataGC_F$tempo[i] == 1){
  DataGC_F$n_parasitos[i] <-  500000
  }
}

write.csv(DataGC_F, file = "Data/Processed/DataGC_F.csv")
