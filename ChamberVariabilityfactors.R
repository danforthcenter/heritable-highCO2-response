#This is the script to calculate and store chamber variability factors in memory
library(ggplot2); library(reshape2); library(dplyr); library(plotrix); library(extrafont); library(patchwork)
setwd("~/OneDrive - Donald Danforth Plant Science Center/bioinformatics/uRoLE/Manuscript/Figure 1/")
data <- read.table(file = "AT_areas.tsv",
                   header = TRUE,
                   sep = "\t",
                   col.names = c("area","co2","genotype","BR","DAP","round","co2_genotype","chamber"))
dataSubset <- head(data,n=0)
unique(data$co2_genotype)
#Subset the data that will be used to calculate chamber variability
for(i in c(1:15)){
  data_2 <- data %>%
    filter((genotype=="wtCol" | genotype=="WTlow" | genotype=="WThigh") & DAP==i)
  dataSubset <- rbind(dataSubset,data_2)
}

#Round 1 lowCO2 variability factors (lowCO2 R1 - mean of lowCO2 R1, R2 and R3)
ChamberR1Low <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- dataSubset %>%
    filter((co2_genotype=="lowCo2_wtCol" | (co2_genotype=="lowCo2_WTlow" & round=="R3")) & DAP==i) %>%
    mutate(area2 <- area - median(.$area)) %>%
    filter(co2_genotype=="lowCo2_wtCol" & round=="R1")
  ChamberR1Low <- rbind(ChamberR1Low,data_2)
}
colnames(ChamberR1Low)[9] <- "modarea"
ChamberR1Low <- ChamberR1Low %>%
  group_by(DAP,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

#Round 2 lowCO2 variability factors (lowCO2 R2 - mean of lowCO2 R1, R2 and R3)
ChamberR2Low <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- dataSubset %>%
    filter((co2_genotype=="lowCo2_wtCol" | (co2_genotype=="lowCo2_WTlow" & round=="R3")) & DAP==i) %>%
    mutate(area2 <- area - median(.$area)) %>%
    filter(co2_genotype=="lowCo2_wtCol" & round=="R2")
  ChamberR2Low <- rbind(ChamberR2Low,data_2)
}
colnames(ChamberR2Low)[9] <- "modarea"
ChamberR2Low <- ChamberR2Low %>%
  group_by(DAP,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

#Round 3 lowCO2 Chamber165 variability factors (lowCO2 R3 Chamber 165 - mean of lowCO2 R1, R2 and R3)
ChamberR3Low165 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- dataSubset %>%
    filter((co2_genotype=="lowCo2_wtCol" | (co2_genotype=="lowCo2_WTlow" & round=="R3")) & DAP==i) %>%
    mutate(area2 <- area - median(.$area)) %>%
    filter(co2_genotype=="lowCo2_WTlow" & round=="R3" & chamber=="165")
  ChamberR3Low165 <- rbind(ChamberR3Low165,data_2)
}
colnames(ChamberR3Low165)[9] <- "modarea"
ChamberR3Low165 <- ChamberR3Low165 %>%
  group_by(DAP,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

#Round 3 lowCO2 Chamber153 variability factors (lowCO2 R3 Chamber 153 - mean of lowCO2 R1, R2 and R3)
ChamberR3Low153 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- dataSubset %>%
    filter((co2_genotype=="lowCo2_wtCol" | (co2_genotype=="lowCo2_WTlow" & round=="R3")) & DAP==i) %>%
    mutate(area2 <- area - median(.[which(co2_genotype=="lowCo2_wtCol" | (co2_genotype=="lowCo2_WTlow" & round=="R3")),]$area)) %>%
    filter(co2_genotype=="lowCo2_WTlow" & round=="R3" & chamber=="153")
  ChamberR3Low153 <- rbind(ChamberR3Low153,data_2)
}
colnames(ChamberR3Low153)[9] <- "modarea"
ChamberR3Low153 <- ChamberR3Low153 %>%
  group_by(DAP,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

#Add data for the missing point
trial <- data.frame(1,"R3",0,0,0)
names(trial) <- c("DAP","round","mean","sd","se")
ChamberR3Low153 <- rbind(trial,ChamberR3Low153)

#Round 1 highCO2 variability factors (highCO2 R1 - mean of highCO2 R1 and R2)
ChamberR1High <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- dataSubset %>%
    filter((co2_genotype=="highCo2_wtCol") & DAP==i) %>%
    mutate(area2 <- area - median(.$area)) %>%
    filter(co2_genotype=="highCo2_wtCol" & round=="R1")
  ChamberR1High <- rbind(ChamberR1High,data_2)
}
colnames(ChamberR1High)[9] <- "modarea"
ChamberR1High <- ChamberR1High %>%
  group_by(DAP,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

#Round 2 highCO2 variability factors (highCO2 R2 - mean of highCO2 R1 and R2)
ChamberR2High <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- dataSubset %>%
    filter((co2_genotype=="highCo2_wtCol") & DAP==i) %>%
    mutate(area2 <- area - median(.$area)) %>%
    filter(co2_genotype=="highCo2_wtCol" & round=="R2")
  ChamberR2High <- rbind(ChamberR2High,data_2)
}
colnames(ChamberR2High)[9] <- "modarea"
ChamberR2High <- ChamberR2High %>%
  group_by(DAP,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

#Plot how wtCol changed.
#Before correction wtCol plot.
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter((genotype=="wtCol" | genotype=="WTlow") & DAP==i)
  datawtCol <- rbind(datawtCol,data_2)
}
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round,chamber) %>%
  summarise_at(vars("area"),funs(mean,sd,se=std.error))
#data_aggregated$sample <- rep("wtCol_lowCO2_R1",times=nrow(data_aggregated))
data_plot <- data_aggregated
data_plot$co2_genotype_round <- paste(data_plot$co2_genotype,"_",data_plot$round,"_",data_plot$chamber)
data_plot$DAP <- data_plot$DAP + 7
ggplot(data_plot,aes(x=DAP,y=mean,ymax=mean+se,ymin=mean-se))+
  geom_line(aes(color=co2_genotype_round))+
  geom_ribbon(aes(fill=co2_genotype_round),alpha=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0,hjust=0.5),
        text = element_text(size = 12, family = "Arial"),
        plot.title = element_text(hjust=0.5))+
  labs(title="Chamber variability before batch correction", y = "Area (pixel2)")

#After correction wtCol plot.
#R1 low CO2
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="lowCo2_wtCol" & round=="R1" & DAP==i) %>%
    mutate(area2 <- area - ChamberR1Low[which(ChamberR1Low$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$sample <- rep("wtCol_lowCO2_R1_mod",times=nrow(data_aggregated))
data_plot <- data_aggregated

#R2 low CO2
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="lowCo2_wtCol" & round=="R2" & DAP==i) %>%
    mutate(area2 <- area - ChamberR2Low[which(ChamberR2Low$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$sample <- rep("wtCol_lowCO2_R2_mod",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#R3 low CO2 - Chamber 165
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="lowCo2_WTlow" & round=="R3" & chamber=="165" & DAP==i) %>%
    mutate(area2 <- area - ChamberR3Low165[which(ChamberR3Low165$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$sample <- rep("wtCol_lowCO2_R3-165_mod",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#R3 low CO2 - Chamber 153
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="lowCo2_WTlow" & round=="R3" & chamber=="153" & DAP==i) %>%
    mutate(area2 <- area - ChamberR3Low153[which(ChamberR3Low153$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$sample <- rep("wtCol_lowCO2_R3-153_mod",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#R1 high CO2
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="highCo2_wtCol" & round=="R1" & DAP==i) %>%
    mutate(area2 <- area - ChamberR1High[which(ChamberR1High$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$sample <- rep("wtCol_highCO2_R1_mod",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#R2 high CO2
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="highCo2_wtCol" & round=="R2" & DAP==i) %>%
    mutate(area2 <- area - ChamberR2High[which(ChamberR2High$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$sample <- rep("wtCol_highCO2_R2_mod",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)
data_plot$DAP <- data_plot$DAP + 7
ggplot(data_plot,aes(x=DAP,y=mean,ymax=mean+se,ymin=mean-se))+
  geom_line(aes(color=sample))+
  geom_ribbon(aes(fill=sample),alpha=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0,hjust=0.5),
        text = element_text(size = 12, family = "Arial"),
        plot.title = element_text(hjust=0.5))+
  labs(title="Batch corrected high CO2 vs ambient CO2", y = "Area (pixel2)")+
  coord_cartesian(ylim = c(0,12000))
