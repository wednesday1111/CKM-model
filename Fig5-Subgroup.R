rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(readxl)
library(ggplot2)
library(ggsci)

auc <- read_excel("Subgroup.xlsx")
auc$Set[auc$Set=='Training']<-'Training set'
auc$Set[auc$Set=='Validation']<-'Validation set'
auc$Type[auc$Type=='+dietary antioxidant features']<-'Modelling plus dietary antioxidant features'
auc$Type[auc$Type=='Baseline features']<-'Modelling using baseline features'
seq<-auc$Subgroup[1:35]
auc$Subgroup<-factor(auc$Subgroup,levels =rev(seq))
auc$Model<-factor(auc$Model,levels =c('RF','GBM'))

p <- ggplot(auc, aes(x = AUC, y = Subgroup,colour = Type)) +
  geom_point(size=4)+
  scale_color_manual(values=c('Modelling using baseline features'="#4575b4",
                              'Modelling plus dietary antioxidant features'='#BD6263'))+
  facet_grid(.~Set+Model)+
  labs(x = 'AUC', y = '')+
  theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(size=0.5,fill = 'transparent'), 
    axis.line = element_blank(), 
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', color = "gray"), 
    axis.ticks = element_line(size = 0.7), 
    axis.ticks.length = unit(1.5, "mm"), 
    legend.title = element_blank(),
    legend.position = 'top', 
    axis.text = element_text(size = 15), 
    legend.text = element_text(size = 13), 
    axis.title = element_text(size = 15), 
    strip.text.x = element_text(size = 15,face = "bold"),
    strip.background.x = element_rect(fill = "grey90")
  )
p

tiff(filename = "output/Subgroup.tiff", 
     width = 11,height = 11,units = "in",res = 300)
p
dev.off()   
