library(ggplot2); library(reshape2); library(dplyr); library(plotrix); library(extrafont); library(patchwork)
data <- read.table(file = "AT_areas.tsv",
                   header = TRUE,
                   sep = "\t",
                   col.names = c("area","co2","genotype","BR","DAP","round","co2_genotype","chamber"))

#Figure 5B - wtCol and mutants - ago1, ubp1b, suvh4/5/6 and met1

#wtCol
datawtColR1 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="wtCol" & round=="R1" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R1"),]$area) -
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean +
             ChamberR1Low[which(ChamberR1Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R1")
  datawtColR1 <- rbind(datawtColR1,data_2)}

colnames(datawtColR1)[9] <- "modarea"

data_aggregated <- datawtColR1 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("wtCol_diff_R1",times=nrow(data_aggregated))
data_plot <- data_aggregated

#suvh456
datasuvh456R2 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="suvh456" & round=="R2" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R2"),]$area) -
             ChamberR2High[which(ChamberR2High$DAP==i),]$mean +
             ChamberR2Low[which(ChamberR2Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R2")
  datasuvh456R2 <- rbind(datasuvh456R2,data_2)}
colnames(datasuvh456R2)[9] <- "modarea"

data_aggregated <- datasuvh456R2 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("suvh456_diff_R2",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#ubp1
dataubp1R2 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="ubp1" & round=="R2" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R2"),]$area) -
             ChamberR2High[which(ChamberR2High$DAP==i),]$mean +
             ChamberR2Low[which(ChamberR2Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R2")
  dataubp1R2 <- rbind(dataubp1R2,data_2)}
colnames(dataubp1R2)[9] <- "modarea"

data_aggregated <- dataubp1R2 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("ubp1_diff_R2",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#ago1
dataago1R2 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="ago1" & round=="R2" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R2"),]$area) -
             ChamberR2High[which(ChamberR2High$DAP==i),]$mean +
             ChamberR2Low[which(ChamberR2Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R2")
  dataago1R2 <- rbind(dataago1R2,data_2)}

colnames(dataago1R2)[9] <- "modarea"

data_aggregated <- dataago1R2 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("ago1_diff_R2",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#met1
datamet1R2 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="met1" & round=="R2" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R2"),]$area) -
             ChamberR2High[which(ChamberR2High$DAP==i),]$mean +
             ChamberR2Low[which(ChamberR2Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R2")
  datamet1R2 <- rbind(datamet1R2,data_2)}
colnames(datamet1R2)[9] <- "modarea"

data_aggregated <- datamet1R2 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("met1_diff_R2",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)
data_plot$DAP <- data_plot$DAP + 7
ggplot(data_plot,aes(x=DAP,y=mean,ymax=mean+se,ymin=mean-se))+
  geom_line(aes(color=co2_genotype))+
  geom_ribbon(aes(fill=co2_genotype),alpha=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0,hjust=0.5),
        text = element_text(size = 12, family = "Arial"),
        plot.title = element_text(hjust=0.5))+
  labs(title="High CO2 response in mutants", y = "Change in Plant Area (pixel2)")+
  coord_cartesian(ylim = c(-200,2800))+
  scale_color_discrete(name="genotype", labels=c("Wt","ago1","ubp1b","suvh4/5/6","met1"))+
  scale_fill_discrete(name="genotype", labels=c("Wt","ago1","ubp1b","suvh4/5/6","met1"))


#Figure 5C - wtCol and mutants - ddm1, pol IV, cmt2/3

#wtCol
datawtColR1 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="wtCol" & round=="R1" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R1"),]$area) -
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean +
             ChamberR1Low[which(ChamberR1Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R1")
  datawtColR1 <- rbind(datawtColR1,data_2)}

colnames(datawtColR1)[9] <- "modarea"

data_aggregated <- datawtColR1 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("wtCol_diff_R1",times=nrow(data_aggregated))
data_plot <- data_aggregated

#pol4
datapol4R2 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="pol4" & round=="R2" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R2"),]$area) -
             ChamberR2High[which(ChamberR2High$DAP==i),]$mean +
             ChamberR2Low[which(ChamberR2Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R2")
  datapol4R2 <- rbind(datapol4R2,data_2)}
colnames(datapol4R2)[9] <- "modarea"

data_aggregated <- datapol4R2 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("pol4_diff_R2",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#ddm1
dataddm1R1 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="ddm1" & round=="R1" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R1"),]$area) -
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean +
             ChamberR1Low[which(ChamberR1Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R1")
  dataddm1R1 <- rbind(dataddm1R1,data_2)}

colnames(dataddm1R1)[9] <- "modarea"

data_aggregated <- dataddm1R1 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("ddm1_diff_R1",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#cmt23
datacmt23R1 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(genotype=="cmt23" & round=="R1" & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2=="lowCo2" & .$round=="R1"),]$area) -
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean +
             ChamberR1Low[which(ChamberR1Low$DAP==i),]$mean) %>%
    filter(co2=="highCo2" & round=="R1")
  datacmt23R1 <- rbind(datacmt23R1,data_2)}

colnames(datacmt23R1)[9] <- "modarea"

data_aggregated <- datacmt23R1 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("cmt23_diff_R1",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)
data_plot$DAP <- data_plot$DAP + 7
ggplot(data_plot,aes(x=DAP,y=mean,ymax=mean+se,ymin=mean-se))+
  geom_line(aes(color=co2_genotype))+
  geom_ribbon(aes(fill=co2_genotype),alpha=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0,hjust=0.5),
        text = element_text(size = 12, family = "Arial"),
        plot.title = element_text(hjust=0.5))+
  labs(title="High CO2 response in mutants", y = "Change in Plant Area (pixel2)")+
  coord_cartesian(ylim = c(-200,2800))+
  scale_color_discrete(name="genotype", labels=c("Wt","ddm1","pol IV","cmt2/3"))+
  scale_fill_discrete(name="genotype", labels=c("Wt","ddm1","pol IV","cmt2/3"))
