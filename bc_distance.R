library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(FSA)
library(pairwiseAdonis)
library(cowplot)
library(ggvenn)
##bc距离
ingroup <- as.matrix(distance)
dec <- as.vector(ingroup[1:10,1:10])
dec <- unique(dec);dec <- dec[-1]
jan <- as.vector(ingroup[11:20,11:20])
jan <- unique(jan);jan <- jan[-1]
feb <- as.vector(ingroup[21:30,21:30])
feb <- unique(feb);feb <- feb[-1]
mar <- as.vector(ingroup[31:40,31:40])
mar <- unique(mar);mar <- mar[-1]

ingroup <- data.frame(group=c(rep("Dec",45),rep("Jan",45),rep("Feb",45),rep("Mar",45)),
                      bray_crutis=c(dec,jan,feb,mar))
ingroup$group <- factor(ingroup$group,levels = c("Dec","Jan","Feb","Mar"))
p7 <- ggplot(ingroup,aes(group,bray_crutis,fill=group))+geom_violin()+geom_jitter(alpha=.3,width = .1,)+
  stat_compare_means(method = "kruskal")+
  scale_fill_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = .5))+guides(fill="none")
dunnTest(as.numeric(bray_crutis)~group,ingroup,method="bh")