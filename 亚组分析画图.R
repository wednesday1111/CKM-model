rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(readxl)
library(ggplot2)
library(ggsci)

auc <- read_excel("20250225/Subgroup.xlsx")
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
  theme_classic(base_size = 15) + # 设置基本字号为8pt
  theme(panel.border = element_rect(size=0.5,fill = 'transparent'), # 添加全框线
    axis.line = element_blank(), # 删除原始轴线
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', color = "gray"), # 开启y轴主网格线
    axis.ticks = element_line(size = 0.7), #设置刻度线粗细与颜色
    axis.ticks.length = unit(1.5, "mm"), # 设置刻度线的长度
    legend.title = element_blank(), # 设置图例标题为空
    legend.position = 'top', # 设置图例位置
    axis.text = element_text(size = 15), # 设置坐标轴标签字号和颜色
    legend.text = element_text(size = 13), # 设置图例文本字号
    axis.title = element_text(size = 15), # 设置坐标轴标题字号
    strip.text.x = element_text(size = 15,face = "bold"),
    strip.background.x = element_rect(fill = "grey90")
  )
p

tiff(filename = "output/Subgroup.tiff", 
     width = 11,height = 11,units = "in",res = 300)
p
dev.off()   
