library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(SimilarityMeasures)
data <- read_delim("pedestrian.csv", delim = ",")

ped_1 <- data %>%
  subset(TrajID == 1)
ped_2 <- data %>%
  subset(TrajID == 2)
ped_3 <- data %>%
  subset(TrajID == 3)
ped_4 <- data %>%
  subset(TrajID == 4)
ped_5 <- data %>%
  subset(TrajID == 5)
ped_6 <- data %>%
  subset(TrajID == 6)

p1 <- ped_1 %>%
  ggplot(aes(E,N)) +
  geom_point(col = "red")+
  geom_path()
p2 <-ped_2 %>%
  ggplot(aes(E,N)) +
  geom_point(col = "yellow")+
  geom_path()
p3 <- ped_3 %>%
  ggplot(aes(E,N)) +
  geom_point(col = "blue")+
  geom_path()
p4 <- ped_4 %>%
  ggplot(aes(E,N)) +
  geom_point(col = "lightblue")+
  geom_path()
p5 <-ped_6 %>%
  ggplot(aes(E,N)) +
  geom_point(col = "cyan")+
  geom_path()
p6 <-ped_6 %>%
  ggplot(aes(E,N)) +
  geom_point(col = "green")+
  geom_path()

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

traj_list_ped_1 <- as.matrix(cbind(ped_1$E,ped_1$N))
traj_list_ped_2 <- as.matrix(cbind(ped_2$E,ped_2$N))
traj_list_ped_3 <- as.matrix(cbind(ped_3$E,ped_3$N))
traj_list_ped_4 <- as.matrix(cbind(ped_4$E,ped_4$N))
traj_list_ped_5 <- as.matrix(cbind(ped_5$E,ped_5$N))
traj_list_ped_6 <- as.matrix(cbind(ped_6$E,ped_6$N))

dtw_1_2 <- DTW(traj_list_ped_1,traj_list_ped_2)
dtw_1_3 <- DTW(traj_list_ped_1,traj_list_ped_3)
dtw_1_4 <- DTW(traj_list_ped_1,traj_list_ped_4)
dtw_1_5 <- DTW(traj_list_ped_1,traj_list_ped_5)
dtw_1_6 <- DTW(traj_list_ped_1,traj_list_ped_6)

edi_1_2 <- EditDist(traj_list_ped_1,traj_list_ped_2)
edi_1_3 <- EditDist(traj_list_ped_1,traj_list_ped_3)
edi_1_4 <- EditDist(traj_list_ped_1,traj_list_ped_4)
edi_1_5 <- EditDist(traj_list_ped_1,traj_list_ped_5)
edi_1_6 <- EditDist(traj_list_ped_1,traj_list_ped_6)

fre_1_2 <- Frechet(traj_list_ped_1,traj_list_ped_2)
fre_1_3 <- Frechet(traj_list_ped_1,traj_list_ped_3)
fre_1_4 <- Frechet(traj_list_ped_1,traj_list_ped_4)
fre_1_5 <- Frechet(traj_list_ped_1,traj_list_ped_5)
fre_1_6 <- Frechet(traj_list_ped_1,traj_list_ped_6)

lcss_1_2 <- LCSS(traj_list_ped_1,traj_list_ped_2)
lcss_1_3 <- LCSS(traj_list_ped_1,traj_list_ped_3)
lcss_1_4 <- LCSS(traj_list_ped_1,traj_list_ped_4)
lcss_1_5 <- LCSS(traj_list_ped_1,traj_list_ped_5)
lcss_1_6 <- LCSS(traj_list_ped_1,traj_list_ped_6)


hist(dtw_1_2,dtw_1_3,dtw_1_4,dtw_1_5,dtw_1_6)





