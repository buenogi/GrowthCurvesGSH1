################################################################################
############## Data cleaning  - Growth Curves GSH1 HKO clones  #################
################################################################################

library(dplyr)
library(ggplot2)


#Loading data
DataGC <- read.csv(file = "Data/Processed/DataGC.csv")

# Checking classes

sapply(DataGC, class)

DataGC$experiment <- as.factor(DataGC$experiment)
DataGC$conc <- as.factor(DataGC$conc)

# EDA

DataGC$tempo <- as.factor(DataGC$tempo)

DataGC_SUM_exp <- DataGC%>%
  group_by(pop, conc, tempo, experiment)%>%
  summarise(abs_exp = mean(abs), sd_value_abs_exp = sd(abs))

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
  summarise(mean_value = mean(abs), sd_value = sd(abs))

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
  ggtitle("Promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs (600nm)")+
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

# Remoção do dia zero:

DataGC_def <- DataGC%>%
  filter(tempo!= 1)

DataGC_def$conc <-  as.factor(DataGC_def$conc)

# Sumarização
DataGC_def$pop <- as.factor(DataGC_def$pop)
DataGC_sum <- DataGC_def%>%
  group_by(pop, conc, tempo)%>%
  summarise(mean_value = mean(abs), sd_value = sd(abs))

DataGC_error <- left_join(DataGC_def, DataGC_sum)

# Gŕafico de linhas com barras de erro

P1 <- DataGC_error%>%
  mutate(pop = factor(pop, levels = c("REF", "GSH1", "C6", "C7","C44",
                                      "C58","C67","C73", "C85", "C89")))%>%
ggplot() +
  aes(x = tempo, y = mean_value, colour = pop, group = pop) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymax = mean_value + sd_value/sqrt(3),
                    ymin  = mean_value - sd_value/sqrt(3)), 
                width = 0.05)+
  scale_color_manual(values = c("deeppink", colorRampPalette(c("lightseagreen", "grey"))(9))) +
  scale_x_discrete(labels = c("24h", "48h", "72h", "96h"))+
  labs(x = "Tempo",
       y = "Abs (600nm)",
       color = "Clones")+
  theme_bw() +
  facet_wrap(vars(conc),labeller = labeller(conc = c("0" = "0 μM","150" = "150 μM")))+
  theme(text = element_text(size = 18),
        legend.position = "bottom")
P1
ggsave("Figuras/04_GrowthC_plot.png")

P2 <- DataGC_error%>%
  mutate(pop = factor(pop, levels = c("REF", "GSH1", "C6", "C7","C44",
                                      "C58","C67","C73", "C85", "C89")))%>%
  ggplot() +
  aes(x = tempo, y = mean_value, group = conc, color = conc) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymax = mean_value + sd_value/sqrt(3),
                    ymin = mean_value - sd_value/sqrt(3)), width = 0.1)+
  facet_wrap(vars(pop), nrow = 2)+
  labs(x = "Tempo",
       y = "Abs (600nm)",
       color = "Concentração de SbIII (μM)")+
  scale_x_discrete(labels = c("24h", "48h", "72h", "96h"))+
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "bottom")
P2
ggsave("Figuras/05_GrowthC_plot.png")  

library(patchwork)

P1/P2 + plot_annotation(tag_levels = "A")

ggsave("Figuras/6_GrowthC_plot.png")  
