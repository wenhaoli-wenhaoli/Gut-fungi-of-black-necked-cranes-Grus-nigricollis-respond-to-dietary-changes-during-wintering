library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(FSA)
library(pairwiseAdonis)
library(cowplot)
library(ggvenn)
##稀疏性曲线
asv <- feature_table[,8:47]
rarecurve(t(asv), step = 5000,col = "blue", cex = 0.6)
sequence_depth <- c()
asv_number <- c()
sample <- c()
for (i in seq(0,sum(feature_table$D9),5000)) {
  test <- rrarefy(t(asv),i)
  sequence_depth <- c(sequence_depth,rep(i,40))
  sample <- c(sample,rownames(test))
  for (i in colnames(asv)) {
    asv_number <- c(asv_number,sum(t(test[i,]) != 0))
  }
}

rarefaction_curve <- data.frame(sequence_depth,asv_number,sample)
rarefaction_curve$period <- ""
rarefaction_curve$period[grep("A",rarefaction_curve$sample)] <- "Dec"
rarefaction_curve$period[grep("B",rarefaction_curve$sample)] <- "Jan"
rarefaction_curve$period[grep("C",rarefaction_curve$sample)] <- "Feb"
rarefaction_curve$period[grep("D",rarefaction_curve$sample)] <- "Mar"
p1 <- ggplot(rarefaction_curve,aes(sequence_depth,asv_number,fill=sample,color=period)) + geom_line()+
  scale_color_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = .5),
        legend.background = element_blank())

##a多样性
Shannon <- asv %>% t %>% diversity("shannon")
Simpson <- asv %>% t %>% diversity("simpson")
Chao1 = estimateR(t(asv))[2,]
ACE = estimateR(t(asv))[4,]
da <- cbind(Shannon,Simpson,Chao1,ACE)
da <- cbind(rownames(da),da)
da[grep(pattern = "A",rownames(da)),1] <- "Dec"
da[grep(pattern = "B",rownames(da)),1] <- "Jan"
da[grep(pattern = "C",rownames(da)),1] <- "Feb"
da[grep(pattern = "D",rownames(da)),1] <- "Mar"
da <- data.frame(da)
colnames(da)[1] <- "Months"
da$Months <- factor(da$Months,levels = c("Dec","Jan","Feb","Mar"))
p2 <- ggplot(da, aes(x=Months, y=as.numeric(Shannon),fill=Months)) + 
  geom_boxplot() + stat_compare_means(method = "kruskal")+xlab("")+ylab("Shannon")+
  scale_fill_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = .5))+guides(fill="none")
p3 <- ggplot(da, aes(x=Months, y=as.numeric(Simpson),fill=Months)) + 
  geom_boxplot() + stat_compare_means(method = "kruskal")+xlab("")+ylab("Simpson")+
  scale_fill_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = .5))+guides(fill="none")
p4 <- ggplot(da, aes(x=Months, y=as.numeric(Chao1),fill=Months)) + 
  geom_boxplot() + stat_compare_means(method = "kruskal")+xlab("")+ylab("Chap1")+
  scale_fill_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = .5))+guides(fill="none")
p5 <- ggplot(da, aes(x=Months, y=as.numeric(ACE),fill=Months)) + 
  geom_boxplot() + stat_compare_means(method = "kruskal")+xlab("")+ylab("ACE")+
  scale_fill_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = .5))+guides(fill="none")

dunnTest(as.numeric(Shannon)~Months,da,method="bh")
dunnTest(as.numeric(Simpson)~Months,da,method="bh")
dunnTest(as.numeric(Chao1)~Months,da,method="bh")
dunnTest(as.numeric(ACE)~Months,da,method="bh")


