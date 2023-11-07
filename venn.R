library(ggplot2)
library(tidyverse)
library(reshape2)
library(cowplot)
library(ggvenn)

##ven图
data <- read.delim("D:/Desktop/seq/ITS/抽平/feature_table.txt")
data <- data[,8:47]
data <- data.frame(Dec=rowSums(data[,1:10]),Jan=rowSums(data[,11:20]),
                   Feb=rowSums(data[,21:30]),Mar=rowSums(data[,31:40]))

Dec <- data %>% filter(Dec != 0) %>% rownames()
Jan <- data %>% filter(Jan != 0) %>% rownames()
Feb <- data %>% filter(Feb != 0) %>% rownames()
Mar <- data %>% filter(Mar != 0) %>% rownames()

a <- list(`Dec` = c(Dec),
          `Jan` = c(Jan),
          `Feb` = c(Feb),
          `Mar` = c(Mar))
p1 <- ggvenn(a,show_elements = F,show_percentage = F,
             fill_color = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"),
             fill_alpha = 1,
             stroke_color = "white",stroke_alpha = 0.5,stroke_linetype = 'dashed',
             text_size = 8,set_name_size = 10)

data <- read.delim("clipboard",row.names = 1)
data <- sweep(data,2,colSums(data),"/")
pathgen <- data[grep("Animal Pathogen",rownames(data)),]
pathgen <- data.frame(colSums(pathgen),c(rep("Dec",10),rep("Jan",10),rep("Feb",10),rep("Mar",10)))
colnames(pathgen) <- c("Relative_abundace","months")

pathgen$months <- factor(pathgen$months,levels = c("Dec","Jan","Feb","Mar"))
p2 <- ggplot(pathgen,aes(months,Relative_abundace,fill=months))+
  geom_bar(stat = "summary", fun ="mean", position = position_dodge(),alpha=1,color="black",size=0.5)+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar", colour = "black",width = 0.15,position = position_dodge( .9))+
  stat_compare_means(method = "kruskal")+
  scale_fill_manual(values = c("#d1e6c1","#faf0b6","#e69a9b","#c0dbe5"))+ylab("Animal_Pathogen_Relative_abundace")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,linewidth = .5))+guides(fill="none")
dunnTest(as.numeric(Relative_abundace)~months,pathgen,method="bh")

plot_grid(p1,p2)
ggsave("D:/Desktop/seq/ITS/抽平/venn+guild.pdf",width = 8,height = 6)
