library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(FSA)
library(pairwiseAdonis)
library(cowplot)
library(ggvenn)

##b多样性
data <- as.data.frame(t(asv))
group <- data.frame(rownames(data))
group[grep(group$rownames.data.,pattern = "A",),2] <- "Dec"
group[grep(group$rownames.data.,pattern = "B",),2] <- "Jan"
group[grep(group$rownames.data.,pattern = "C",),2] <- "Feb"
group[grep(group$rownames.data.,pattern = "D",),2] <- "Mar"
adonis <- adonis2(data ~ V2, data = group, permutations = 999, method="bray")
adonis_lab <- paste0("PERMANOVA R2=",round(adonis$R2[1],2),",p=", adonis$`Pr(>F)`[1])
pairwise.adonis(x=data, factors=group$V2, sim.function = "vegdist",sim.method = "bray",p.adjust.m = "BH",reduce = NULL,perm = 999)
distance <- vegdist(data.frame(t(asv)), method = 'bray') 
group <- factor(c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)), labels = c("early","middle1","middle2","late"))
dispersion <- betadisper(distance, group, bias.adjust = FALSE,
                         sqrt.dist = FALSE, add = FALSE)
x <- permutest(dispersion)
betadisper_lab <- paste0("PERMDISP R2=",round(x$tab[1,2]/colSums(x$tab)[2],2),",p=",x$tab$`Pr(>F)`[1])
all_lab <- paste(betadisper_lab,"\n",adonis_lab)
#pcoa <- metaMDS(distance,k =2) 
pcoa <- cmdscale(distance, k = (nrow(t(asv)) - 1), eig = TRUE)
plot_data <- data.frame({pcoa$point})[1:2]
plot_data$Months <- rownames(plot_data)
names(plot_data)[1:2] <- c('PCoA1', 'PCoA2')
plot_data[grep(pattern = "A",rownames(plot_data)),3] <- "Dec"
plot_data[grep(pattern = "B",rownames(plot_data)),3] <- "Jan"
plot_data[grep(pattern = "C",rownames(plot_data)),3] <- "Feb"
plot_data[grep(pattern = "D",rownames(plot_data)),3] <- "Mar"
plot_data$Months <- factor(plot_data$Months,levels = c("Dec","Jan","Feb","Mar"))
eig = pcoa$eig
p6 <- ggplot(data = plot_data, aes(x=PCoA1, y=PCoA2,color = Months))+geom_point(alpha=1,size=3)+
  scale_color_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")) +ggtitle(all_lab)+
  stat_ellipse(level=0.95,show.legend=F,size=0.5,alpha=1)+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = .5),
        panel.spacing.x = unit(0.3,"cm"),
        plot.title = element_text(hjust = 1, vjust=-15),
        legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        legend.key = element_blank(),
        strip.background = element_rect(color = "black",size = 1),
        strip.text = element_text(size = 15),
        strip.placement = "inside",
        aspect.ratio = 1.1)+guides(fill="none")