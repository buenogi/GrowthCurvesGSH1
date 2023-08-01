################################################################################
############## Data cleaning  - Growth Curves GSH1 HKO clones  #################
################################################################################

library(dplyr)
library(ggplot2)


#Loading data
DataGC <- read.csv(file = "Data/Processed/DataGC.csv")

# EDA

DataGC$tempo <- as.factor(DataGC$tempo)

DataGC_SUM_exp <- DataGC%>%
  group_by(pop, conc, tempo, experiment)%>%
  summarise(abs_exp = mean(abs), sd_value_abs_exp = sd(abs))

# Box-plot

GrowthC_boxplot <- ggplot(DataGC_SUM_exp, aes(tempo, log(abs_exp), 
                                              shape = conc, 
                                              color = experiment)) +
  geom_point()+
  ggtitle("Promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(DataGC_SUM_exp$pop, nrow = 2)+
  theme_bw()

GrowthC_boxplot



#Data check

sapply(DataGC, class)
DataGC$pop <- as.factor(DataGC$pop)
DataGC$tempo <- as.numeric(DataGC$tempo)
DataGC$conc <- as.factor(DataGC$conc)
DataGC$experiment <- as.factor(DataGC$experiment)
sapply(DataGC, class)

# Sumarizing

DataGC_sum <- DataGC%>%
  group_by(pop, conc, tempo)%>%
  summarise(mean_value = mean(abs), sd_value = sd(abs))

GrowthC_plot <- ggplot(DataGC_sum, aes(tempo, log(mean_value), group = conc)) +
  geom_line(aes(color = conc))+
  ggtitle("Promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(DataGC_sum$pop, nrow = 2)+
  theme_bw()

GrowthC_plot

# Todas juntas


GrowthC_plot_2 <- ggplot(DataGC_sum, aes(tempo, mean_value, group = pop)) +
  geom_line(aes(color  = pop), size = 0.5)+
  ggtitle("Promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_grid(DataGC_sum$conc)+
  theme_bw()

GrowthC_plot_2 + labs(color = "Populations")
