library(ggplot2); library(reshape2); library(dplyr); library(plotrix); library(extrafont); library(patchwork)
data <- read.table(file = "~/AT_areas.tsv",
                   header = TRUE,
                   sep = "\t",
                   col.names = c("area","co2","genotype","BR","DAP","round","co2_genotype","chamber"))

#WtCol progeny of high CO2 vs parental high CO2
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

#cmt23 progeny of high CO2 vs parental high CO2
datacmt23R31 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(((co2_genotype=="highCo2_cmt23" & round=="R1") | (co2_genotype=="lowCo2_cmt23" & round=="R3")) & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2_genotype=="highCo2_cmt23" & .$round=="R1"),]$area) -
             ChamberR3Low153[which(ChamberR3Low153$DAP==i),]$mean +
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean) %>%
    filter(co2_genotype=="lowCo2_cmt23")
  datacmt23R31 <- rbind(datacmt23R31,data_2)}

colnames(datacmt23R31)[9] <- "modarea"

data_aggregated <- datacmt23R31 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("cmt23-diffR31",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#ddm1 progeny of high CO2 vs parental high CO2
dataddm1R31 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(((co2_genotype=="highCo2_ddm1" & round=="R1") | (co2_genotype=="lowCo2_ddm1" & round=="R3")) & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2_genotype=="highCo2_ddm1" & .$round=="R1"),]$area) -
             ChamberR3Low153[which(ChamberR3Low153$DAP==i),]$mean +
             ChamberR1High[which(ChamberR1High$DAP==i),]$mean) %>%             
    filter(co2_genotype=="lowCo2_ddm1")
  dataddm1R31 <- rbind(dataddm1R31,data_2)}

colnames(dataddm1R31)[9] <- "modarea"

data_aggregated <- dataddm1R31 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("ddm1-diffR31",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)

#pol4 progeny of high CO2 vs parental high CO2
datapol4R32 <- head(data,n=0)
for(i in c(1:15)){
  data_2 <- data %>%
    filter(((co2_genotype=="highCo2_pol4" & round=="R2") | (co2_genotype=="lowCo2_pol4" & round=="R3")) & DAP==i) %>%
    mutate(area2 <- area - mean(.[which(.$co2_genotype=="highCo2_pol4" & .$round=="R2"),]$area) -
             ChamberR3Low165[which(ChamberR3Low165$DAP==i),]$mean +
             ChamberR2High[which(ChamberR2High$DAP==i),]$mean) %>%
    filter(co2_genotype=="lowCo2_pol4")
  datapol4R32 <- rbind(datapol4R32,data_2)}

colnames(datapol4R32)[9] <- "modarea"

data_aggregated <- datapol4R32 %>%
  group_by(DAP,co2_genotype,co2,genotype,round) %>%
  summarise_at(vars("modarea"),funs(mean,sd,se=std.error))

data_aggregated$co2_genotype <- rep("pol4-diffR32",times=nrow(data_aggregated))
data_plot <- rbind(data_plot,data_aggregated)
data_plot$DAP <- data_plot$DAP + 7
ggplot(data_plot,aes(x=DAP,y=mean,ymax=mean+se,ymin=mean-se))+
  geom_line(aes(color=co2_genotype))+
  geom_ribbon(aes(fill=co2_genotype),alpha=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0,hjust=0.5),
        text = element_text(size = 12, family = "Arial"),
        plot.title = element_text(hjust=0.5))+
  labs(title="High CO2 growth heritability", y = "Change in Plant Area (pixel2)")+
  coord_cartesian(ylim = c(-4550,2000))+
  scale_color_discrete(name="genotype", labels=c("Wt","ddm1","pol IV","cmt2/3"))+
  scale_fill_discrete(name="genotype", labels=c("Wt","ddm1","pol IV","cmt2/3"))

