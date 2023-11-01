################################################################################
############## Data cleaning  - Growth Curves GSH1 HKO clones  #################
################################################################################

library(dplyr)
library(ggplot2)


#Loading data
DataGC <- read.csv(file = "Data/Processed/DataGC_N.csv")

# Checking classes

sapply(DataGC, class)

DataGC$experiment <- as.factor(DataGC$experiment)
DataGC$conc <- as.factor(DataGC$conc)
# EDA

DataGC$tempo <- as.factor(DataGC$tempo)

DataGC_SUM_exp <- DataGC%>%
  group_by(pop, conc, tempo, experiment)%>%
  summarise(abs_exp = mean(n_parasitos), sd_value_abs_exp = sd(n_parasitos))

# Box-plot

GrowthC_DotPlot <- ggplot(DataGC_SUM_exp, aes(tempo, abs_exp, 
                                              shape = conc, 
                                              color = experiment)) +
  geom_point()+
  ggtitle("GSH1 Hemi mutants promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs (600nm)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(~pop, nrow = 2)+
  theme_bw()

GrowthC_DotPlot

ggsave("Figuras/01_GrowthC_DotPlot.png")
# Sumarizing

DataGC_sum <- DataGC%>%
  group_by(pop, conc, tempo)%>%
  summarise(mean_value = mean(n_parasitos), sd_value = sd(n_parasitos))

GrowthC_plot_Clone <- ggplot(DataGC_sum, aes(tempo, log(mean_value), group = conc)) +
  geom_line(aes(color = conc))+
  ggtitle("GSH1 Hemi mutants promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs (600nm)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(labels = c("0h", "24h", "48h", "72h", "96h")) +
  facet_wrap(~pop, nrow = 2)+
  theme_bw()

GrowthC_plot_Clone
ggsave("Figuras/02_GrowthC_plot_Clone.png")

# Todas juntas


GrowthC_plot <- ggplot(DataGC_sum, aes(tempo, mean_value, group = pop)) +
  geom_line(aes(color  = pop), size = 0.5, linewidth = 1)+
  ggtitle("Crescimento de promastigotas com e sem SbIII") +
  labs(x = " Tempo (h)  ", y = "Nº de parasitos")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(labels = c("0h", "24h", "48h", "72h", "96h")) +
  facet_wrap(DataGC_sum$conc)+
  theme_bw()

GrowthC_plot + labs(color = "Populations")+
  theme(text = element_text(size = 25))
ggsave("Figuras/03_GrowthC_plot.png")

# Forçando dia zero com a mesmo nº de parasitos

DataGC <- read.csv(file = "Data/Processed/DataGC_F.csv")

DataGC_sum <- DataGC%>%
  group_by(pop, conc, tempo)%>%
  summarise(mean_value = mean(n_parasitos), sd_value = sd(n_parasitos))

GrowthC_plot_Clone <- ggplot(DataGC_sum, aes(tempo, log(mean_value), group = conc)) +
  geom_line(aes(color = conc))+
  ggtitle("GSH1 Hemi mutants promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs (600nm)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(labels = c("0h", "24h", "48h", "72h", "96h")) +
  facet_wrap(~pop, nrow = 2)+
  theme_bw()

GrowthC_plot_Clone


# Todas juntas


GrowthC_plot <- ggplot(DataGC_sum, aes(tempo, mean_value, group = pop)) +
  geom_line(aes(color  = pop), size = 0.5, linewidth = 1)+
  ggtitle("Crescimento de promastigotas com e sem SbIII") +
  labs(x = " Tempo (h)  ", y = "Nº de parasitos")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(labels = c("0h", "24h", "48h", "72h", "96h")) +
  facet_wrap(DataGC_sum$conc)+
  theme_bw()

GrowthC_plot + labs(color = "Populations")+
  theme(text = element_text(size = 25))




GrowthC_plot <- ggplot(DataGC_F, aes(group = pop))+
  geom_line(aes(tempo, n_parasitos, color = pop), size = 0.5, linewidth = 1)+
  geom_point(aes(tempo, n_parasitos, color = pop))+
  ggtitle("Crescimento de promastigotas com e sem SbIII") +
  labs(x = " Tempo (h)  ", y = "Nº de parasitos")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(labels = c("0h", "24h", "48h", "72h", "96h")) +
  facet_wrap(DataGC_sum$conc)+
  theme_bw()

GrowthC_plot 
