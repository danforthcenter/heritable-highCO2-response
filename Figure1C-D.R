library(ggplot2); library(reshape2); library(dplyr); library(plotrix); library(extrafont); library(patchwork)

setwd("~/OneDrive - Donald Danforth Plant Science Center/bioinformatics/uRoLE/Manuscript/Figure 1/")
data <- read.table(file = "AT_areas.tsv",
                   header = TRUE,
                   sep = "\t",
                   col.names = c("area","co2","genotype","BR","DAP","round","co2_genotype","chamber"))

#Figure 1C
#Low CO2
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="lowCo2_wtCol" & round=="R1" & DAP==i) %>%
    mutate(area - ChamberR1Low[which(ChamberR1Low$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$co2_genotype <- rep("wtCol_lowCO2_R1_mod",times=nrow(data_aggregated))
data_plot <- data_aggregated

#High CO2
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="highCo2_wtCol" & round=="R1" & DAP==i) %>%
    mutate(area - ChamberR1High[which(ChamberR1High$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$co2_genotype <- rep("wtCol_highCO2_R1_mod",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#Progeny of highCO2, chamber 1
datawtCol <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="lowCo2_WThigh" & chamber=="165" & DAP==i) %>%
    mutate(area - ChamberR3Low165[which(ChamberR3Low165$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
#Progeny of highCO2, chamber 2
for(i in c(1:15)){
  data_2 <- data %>%
    filter(co2_genotype=="lowCo2_WThigh" & chamber=="153" & DAP==i) %>%
    mutate(area - ChamberR3Low153[which(ChamberR3Low153$DAP==i),]$mean)
  datawtCol <- rbind(datawtCol,data_2)
}
colnames(datawtCol)[9] <- "modarea"
data_aggregated <- datawtCol %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))
data_aggregated$co2_genotype <- rep("wtCol_lowCO2_R3_mod",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)
data_plot$DAP <- data_plot$DAP + 7
p1 <- ggplot(data_plot,aes(x=DAP,y=mean,ymax=mean+se,ymin=mean-se))+
  geom_line(aes(color=co2_genotype))+
  geom_ribbon(aes(fill=co2_genotype),alpha=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0,hjust=0.5),
        text = element_text(size = 12, family = "Arial"),
        plot.title = element_text(hjust=0.5))+
  labs(title="Wt Arabidopsis CO2 growth response", y = "Area")+
  coord_cartesian(ylim = c(0,11500))+
  ylab("Average Plant Area (pixel2)")+
  scale_color_discrete(name="CO2\nCondition", labels=c("high CO2","low CO2","progeny of high CO2"))+
  scale_fill_discrete(name="CO2\nCondition", labels=c("high CO2","low CO2","progeny of high CO2"))
p1

#Figure 1D
datawtColR31 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(((co2_genotype=="highCo2_wtCol" & round=="R1") | (co2_genotype=="lowCo2_WThigh" & chamber=="165")) & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2_genotype=="highCo2_wtCol" & .$round=="R1"),]$area)-
             ChamberR3Low165[which(ChamberR3Low165$DAP==i),]$mean +
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean) %>%
    filter(co2_genotype=="lowCo2_WThigh")
  datawtColR31 <- rbind(datawtColR31,data_2)}

datawtColR3chamber153 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(((co2_genotype=="highCo2_wtCol" & round=="R1") | (co2_genotype=="lowCo2_WThigh" & chamber=="153")) & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2_genotype=="highCo2_wtCol" & .$round=="R1"),]$area)-
             ChamberR3Low153[which(ChamberR3Low153$DAP==i),]$mean +
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean) %>%
    filter(co2_genotype=="lowCo2_WThigh")
  datawtColR3chamber153 <- rbind(datawtColR3chamber153,data_2)}

datawtColR31 <- rbind(datawtColR31,datawtColR3chamber153)
colnames(datawtColR31)[9] <- "modarea"

data_aggregated <- datawtColR31 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("high-low-diffR31",times=nrow(data_aggregated))
data_plot <- data_aggregated
data_plot$DAP <- data_plot$DAP + 7
p2 <- ggplot(data_plot,aes(x=DAP,y=mean,ymax=mean+se,ymin=mean-se))+
  geom_line(aes(color=co2_genotype))+
  geom_ribbon(aes(fill=co2_genotype),alpha=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0,hjust=0.5),
        text = element_text(size = 12, family = "Arial"),
        plot.title = element_text(hjust=0.5))+
  labs(title="High CO2 growth heritability", y = "Area")+
  coord_cartesian(ylim = c(-2000,1800))+
  ylab("Change in Plant Area (pixel2)")+
  scale_color_discrete(name="CO2\nCondition", labels=c("progeny of high CO2 -\nparental high CO2"))+
  scale_fill_discrete(name="CO2\nCondition", labels=c("progeny of high CO2 -\nparental high CO2"))
p2

