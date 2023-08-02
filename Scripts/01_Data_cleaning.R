################################################################################
############## Data cleaning  - Growth Curves GSH1 HKO clones  #################
################################################################################

library(tidyverse)
library(dplyr)

#Loading data

D1_EXP1 <- read.csv(file = "Data/Raw/D1_EXP1.csv", sep = ",")
colnames(D1_EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D1_EXP1 <- D1_EXP1[,-1]
D1_EXP1 <- D1_EXP1[,-1]
D1_EXP1 <- D1_EXP1[,-11]
D1_EXP1 <- D1_EXP1[-1,]
D1_EXP1 <- D1_EXP1[-7,]

D1_EXP1 <- D1_EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D1_EXP1$tempo <- "0" 
D1_EXP1$conc  <- rep(c("0", "150"), each = 30)
D1_EXP1$experiment <- "EXP1"  

D2_EXP1 <- read.csv(file = "Data/Raw/D2_EXP1.csv", sep = ",")
colnames(D2_EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D2_EXP1 <- D2_EXP1[,-1]
D2_EXP1 <- D2_EXP1[,-1]
D2_EXP1 <- D2_EXP1[,-11]
D2_EXP1 <- D2_EXP1[-1,]
D2_EXP1 <- D2_EXP1[-7,]

D2_EXP1 <- D2_EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D2_EXP1$tempo <- "24" 
D2_EXP1$conc  <- rep(c("0", "150"), each = 30)
D2_EXP1$experiment <- "EXP1"  
D3_EXP1 <- read.csv(file = "Data/Raw/D3_EXP1.csv", sep = ",")
colnames(D3_EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D3_EXP1 <- D3_EXP1[,-1]
D3_EXP1 <- D3_EXP1[,-1]
D3_EXP1 <- D3_EXP1[,-11]
D3_EXP1 <- D3_EXP1[-1,]
D3_EXP1 <- D3_EXP1[-7,]

D3_EXP1 <- D3_EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D3_EXP1$tempo <- "48" 
D3_EXP1$conc  <- rep(c("0", "150"), each = 30)
D3_EXP1$experiment <- "EXP1"  
D4_EXP1 <- read.csv(file = "Data/Raw/D4_EXP1.csv", sep = ",")
colnames(D4_EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D4_EXP1 <- D4_EXP1[,-1]
D4_EXP1 <- D4_EXP1[,-1]
D4_EXP1 <- D4_EXP1[,-11]
D4_EXP1 <- D4_EXP1[-1,]
D4_EXP1 <- D4_EXP1[-7,]

D4_EXP1 <- D4_EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D4_EXP1$tempo <- "72" 
D4_EXP1$conc  <- rep(c("0", "150"), each = 30)
D4_EXP1$experiment <- "EXP1"  
D5_EXP1 <- read.csv(file = "Data/Raw/D5_EXP1.csv", sep = ",")
colnames(D5_EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D5_EXP1 <- D5_EXP1[,-1]
D5_EXP1 <- D5_EXP1[,-1]
D5_EXP1 <- D5_EXP1[,-11]
D5_EXP1 <- D5_EXP1[-1,]
D5_EXP1 <- D5_EXP1[-7,]

D5_EXP1 <- D5_EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D5_EXP1$tempo <- "96" 
D5_EXP1$conc  <- rep(c("0", "150"), each = 30)
D5_EXP1$experiment <- "EXP1"  

#---------------Experiment 2


D1_EXP2 <- read.csv(file = "Data/Raw/D1_EXP2.csv", sep = ",")
colnames(D1_EXP2) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D1_EXP2 <- D1_EXP2[,-1]
D1_EXP2 <- D1_EXP2[,-1]
D1_EXP2 <- D1_EXP2[,-11]
D1_EXP2 <- D1_EXP2[-1,]
D1_EXP2 <- D1_EXP2[-7,]

D1_EXP2 <- D1_EXP2 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D1_EXP2$tempo <- "0" 
D1_EXP2$conc  <- rep(c("0", "150"), each = 30)
D1_EXP2$experiment <- "EXP2"  

D2_EXP2 <- read.csv(file = "Data/Raw/D2_EXP2.csv", sep = ",")
colnames(D2_EXP2) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D2_EXP2 <- D2_EXP2[,-1]
D2_EXP2 <- D2_EXP2[,-1]
D2_EXP2 <- D2_EXP2[,-11]
D2_EXP2 <- D2_EXP2[-1,]
D2_EXP2 <- D2_EXP2[-7,]

D2_EXP2 <- D2_EXP2 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D2_EXP2$tempo <- "24" 
D2_EXP2$conc  <- rep(c("0", "150"), each = 30)
D2_EXP2$experiment <- "EXP2"  
D3_EXP2 <- read.csv(file = "Data/Raw/D3_EXP2.csv", sep = ",")
colnames(D3_EXP2) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D3_EXP2 <- D3_EXP2[,-1]
D3_EXP2 <- D3_EXP2[,-1]
D3_EXP2 <- D3_EXP2[,-11]
D3_EXP2 <- D3_EXP2[-1,]
D3_EXP2 <- D3_EXP2[-7,]

D3_EXP2 <- D3_EXP2 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D3_EXP2$tempo <- "48" 
D3_EXP2$conc  <- rep(c("0", "150"), each = 30)
D3_EXP2$experiment <- "EXP2"  
D4_EXP2 <- read.csv(file = "Data/Raw/D4_EXP2.csv", sep = ",")
colnames(D4_EXP2) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D4_EXP2 <- D4_EXP2[,-1]
D4_EXP2 <- D4_EXP2[,-1]
D4_EXP2 <- D4_EXP2[,-11]
D4_EXP2 <- D4_EXP2[-1,]
D4_EXP2 <- D4_EXP2[-7,]

D4_EXP2 <- D4_EXP2 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D4_EXP2$tempo <- "72" 
D4_EXP2$conc  <- rep(c("0", "150"), each = 30)
D4_EXP2$experiment <- "EXP2"  
D5_EXP2 <- read.csv(file = "Data/Raw/D5_EXP2.csv", sep = ",")
colnames(D5_EXP2) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D5_EXP2 <- D5_EXP2[,-1]
D5_EXP2 <- D5_EXP2[,-1]
D5_EXP2 <- D5_EXP2[,-11]
D5_EXP2 <- D5_EXP2[-1,]
D5_EXP2 <- D5_EXP2[-7,]

D5_EXP2 <- D5_EXP2 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D5_EXP2$tempo <- "96" 
D5_EXP2$conc  <- rep(c("0", "150"), each = 30)
D5_EXP2$experiment <- "EXP2"  

#--------------Experiment 3


D1_EXP3 <- read.csv(file = "Data/Raw/D1_EXP3.csv", sep = ",")
colnames(D1_EXP3) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D1_EXP3 <- D1_EXP3[,-1]
D1_EXP3 <- D1_EXP3[,-1]
D1_EXP3 <- D1_EXP3[,-11]
D1_EXP3 <- D1_EXP3[-1,]
D1_EXP3 <- D1_EXP3[-7,]

D1_EXP3 <- D1_EXP3 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D1_EXP3$tempo <- "0" 
D1_EXP3$conc  <- rep(c("0", "150"), each = 30)
D1_EXP3$experiment <- "EXP3"  

D2_EXP3 <- read.csv(file = "Data/Raw/D2_EXP3.csv", sep = ",")
colnames(D2_EXP3) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D2_EXP3 <- D2_EXP3[,-1]
D2_EXP3 <- D2_EXP3[,-1]
D2_EXP3 <- D2_EXP3[,-11]
D2_EXP3 <- D2_EXP3[-1,]
D2_EXP3 <- D2_EXP3[-7,]

D2_EXP3 <- D2_EXP3 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D2_EXP3$tempo <- "24" 
D2_EXP3$conc  <- rep(c("0", "150"), each = 30)
D2_EXP3$experiment <- "EXP3"  
D3_EXP3 <- read.csv(file = "Data/Raw/D3_EXP3.csv", sep = ",")
colnames(D3_EXP3) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D3_EXP3 <- D3_EXP3[,-1]
D3_EXP3 <- D3_EXP3[,-1]
D3_EXP3 <- D3_EXP3[,-11]
D3_EXP3 <- D3_EXP3[-1,]
D3_EXP3 <- D3_EXP3[-7,]

D3_EXP3 <- D3_EXP3 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D3_EXP3$tempo <- "48" 
D3_EXP3$conc  <- rep(c("0", "150"), each = 30)
D3_EXP3$experiment <- "EXP3"  
D4_EXP3 <- read.csv(file = "Data/Raw/D4_EXP3.csv", sep = ",")
colnames(D4_EXP3) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D4_EXP3 <- D4_EXP3[,-1]
D4_EXP3 <- D4_EXP3[,-1]
D4_EXP3 <- D4_EXP3[,-11]
D4_EXP3 <- D4_EXP3[-1,]
D4_EXP3 <- D4_EXP3[-7,]

D4_EXP3 <- D4_EXP3 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D4_EXP3$tempo <- "72" 
D4_EXP3$conc  <- rep(c("0", "150"), each = 30)
D4_EXP3$experiment <- "EXP3"  
D5_EXP3 <- read.csv(file = "Data/Raw/D5_EXP3.csv", sep = ",")
colnames(D5_EXP3) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D5_EXP3 <- D5_EXP3[,-1]
D5_EXP3 <- D5_EXP3[,-1]
D5_EXP3 <- D5_EXP3[,-11]
D5_EXP3 <- D5_EXP3[-1,]
D5_EXP3 <- D5_EXP3[-7,]

D5_EXP3 <- D5_EXP3 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D5_EXP3$tempo <- "96" 
D5_EXP3$conc  <- rep(c("0", "150"), each = 30)
D5_EXP3$experiment <- "EXP3"  

#----------------Experiment 4

D1_EXP4 <- read.csv(file = "Data/Raw/D1_EXP4.csv", sep = ",")
colnames(D1_EXP4) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D1_EXP4 <- D1_EXP4[,-1]
D1_EXP4 <- D1_EXP4[,-1]
D1_EXP4 <- D1_EXP4[,-11]
D1_EXP4 <- D1_EXP4[-1,]
D1_EXP4 <- D1_EXP4[-7,]

D1_EXP4 <- D1_EXP4 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D1_EXP4$tempo <- "0" 
D1_EXP4$conc  <- rep(c("0", "150"), each = 30)
D1_EXP4$experiment <- "EXP1"  

D2_EXP1 <- read.csv(file = "Data/Raw/D2_EXP1.csv", sep = ",")
colnames(D2_EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D2_EXP1 <- D2_EXP1[,-1]
D2_EXP1 <- D2_EXP1[,-1]
D2_EXP1 <- D2_EXP1[,-11]
D2_EXP1 <- D2_EXP1[-1,]
D2_EXP1 <- D2_EXP1[-7,]

D2_EXP1 <- D2_EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D2_EXP1$tempo <- "24" 
D2_EXP1$conc  <- rep(c("0", "150"), each = 30)
D2_EXP1$experiment <- "EXP4"  
D3_EXP4 <- read.csv(file = "Data/Raw/D3_EXP4.csv", sep = ",")
colnames(D3_EXP4) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D3_EXP4 <- D3_EXP4[,-1]
D3_EXP4 <- D3_EXP4[,-1]
D3_EXP4 <- D3_EXP4[,-11]
D3_EXP4 <- D3_EXP4[-1,]
D3_EXP4 <- D3_EXP4[-7,]

D3_EXP4 <- D3_EXP4 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D3_EXP4$tempo <- "48" 
D3_EXP4$conc  <- rep(c("0", "150"), each = 30)
D3_EXP4$experiment <- "EXP4"  
D4_EXP4 <- read.csv(file = "Data/Raw/D4_EXP4.csv", sep = ",")
colnames(D4_EXP4) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D4_EXP4 <- D4_EXP4[,-1]
D4_EXP4 <- D4_EXP4[,-1]
D4_EXP4 <- D4_EXP4[,-11]
D4_EXP4 <- D4_EXP4[-1,]
D4_EXP4 <- D4_EXP4[-7,]

D4_EXP4 <- D4_EXP4 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D4_EXP4$tempo <- "72" 
D4_EXP4$conc  <- rep(c("0", "150"), each = 30)
D4_EXP4$experiment <- "EXP4"  
D5_EXP4 <- read.csv(file = "Data/Raw/D5_EXP4.csv", sep = ",")
colnames(D5_EXP4) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                        "C85", "C89", "x2")
D5_EXP4 <- D5_EXP4[,-1]
D5_EXP4 <- D5_EXP4[,-1]
D5_EXP4 <- D5_EXP4[,-11]
D5_EXP4 <- D5_EXP4[-1,]
D5_EXP4 <- D5_EXP4[-7,]

D5_EXP4 <- D5_EXP4 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='abs')
D5_EXP4$tempo <- "96" 
D5_EXP4$conc  <- rep(c("0", "150"), each = 30)
D5_EXP4$experiment <- "EXP4"  




#----------------Binding data

DataGC <- rbind(D1_EXP1,D1_EXP2, D1_EXP3,D1_EXP4, D2_EXP1, D2_EXP2, D2_EXP3,
                D2_EXP4,D3_EXP1, D3_EXP2, D3_EXP3,D3_EXP4, D4_EXP1, D4_EXP2,
                D4_EXP3,D4_EXP4,D5_EXP1, D5_EXP2, D5_EXP3,D5_EXP4)
write.csv(DataGC, file = "Data/Processed/DataGC.csv", sep = ",")

