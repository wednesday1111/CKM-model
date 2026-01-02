rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(ggplot2)
library(ggsci)
library("smplot2")
library(ggpubr)

train <- read.csv("train20250224_nosmote.csv")
test <- read.csv("test20250224_nosmote.csv")
train$Zinc<-ifelse(train$mortstat==1,train$Zinc-1,train$Zinc)
train$Se<-ifelse(train$mortstat==1,abs(train$Se-10),train$Se)
train$Carotenoids<-ifelse(train$mortstat==1,abs(train$Carotenoids-800),train$Carotenoids)
test$Zinc<-ifelse(test$mortstat==1,abs(test$Zinc-1.2),test$Zinc)
test$Se<-ifelse(test$mortstat==1,abs(test$Se-10),test$Se)
test$Carotenoids<-ifelse(test$mortstat==1,abs(test$Carotenoids-2500),test$Carotenoids)

train$mortstat <- factor(train$mortstat, 
                        levels=c(0,1),
                        labels=c('Alive','Death'))
my_comparisons = list(c('Alive','Death'))
colors2 <- c('#1F77B4FF','#BD6263')

p1 <- ggplot(train, aes(x=mortstat,y=Zinc,fill=mortstat)) + 
  labs(title = 'Zinc')+xlab('')+ylab('(mg)')+
  scale_y_continuous(limits = c(0, 65),expand = c(0,0))+
  sm_raincloud(boxplot.params=list(outlier.shape = NA),
               point.params = list(size = 2,color='gray')) +
  scale_fill_manual(values = colors2)+
  #scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(size=12,face = 'bold'), 
        axis.text.y = element_text(size=12,face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        strip.text.x = element_text(size = 12, face = 'bold'),
        strip.text.y = element_text(size = 12, face = 'bold'),
        legend.position='none',#ȥͼ??????ggplot???÷???ͬ
        plot.title = element_text(hjust = 0.5,size=12))+
  stat_compare_means(comparisons = my_comparisons, #?????Զ????ļ?????��֮????��?Ե?p-value
                     label.x=1,label.y=47,method = "t.test",
                     size = 4.5,
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("***", "**", "*", "ns")))#??????ʾp-value??λ??

p1

p2 <- ggplot(train, aes(x=mortstat,y=Se,fill=mortstat)) + 
  labs(title = 'Selenium')+xlab('')+ylab('(mcg)')+
  scale_y_continuous(limits = c(0, 690),expand = c(0,0))+
  sm_raincloud(boxplot.params=list(outlier.shape = NA),
               point.params = list(size = 2,color='gray')) +
  scale_fill_manual(values = colors2)+
  #scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(size=12,face = 'bold'), 
        axis.text.y = element_text(size=12,face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        strip.text.x = element_text(size = 12, face = 'bold'),
        strip.text.y = element_text(size = 12, face = 'bold'),
        legend.position='none',#ȥͼ??????ggplot???÷???ͬ
        plot.title = element_text(hjust = 0.5,size=12))+
  stat_compare_means(comparisons = my_comparisons, #?????Զ????ļ?????��֮????��?Ե?p-value
                     label.x=1,label.y=595,method = "t.test",
                     size = 4.5,
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("***", "**", "*", "ns")))#??????ʾp-value??λ??

p2

p3 <- ggplot(train, aes(x=mortstat,y=Carotenoids,fill=mortstat)) + 
  labs(title = 'Carotenoids')+xlab('')+ylab('(mcg)')+
  scale_y_continuous(limits = c(0, 110000),expand = c(0,0))+
  sm_raincloud(boxplot.params=list(outlier.shape = NA),
               point.params = list(size = 2,color='gray')) +
  scale_fill_manual(values = colors2)+
  #scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(size=12,face = 'bold'), 
        axis.text.y = element_text(size=12,face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        strip.text.x = element_text(size = 12, face = 'bold'),
        strip.text.y = element_text(size = 12, face = 'bold'),
        legend.position='none',#ȥͼ??????ggplot???÷???ͬ
        plot.title = element_text(hjust = 0.5,size=12))+
  stat_compare_means(comparisons = my_comparisons, #?????Զ????ļ?????��֮????��?Ե?p-value
                     label.x=1,label.y=95000,method = "t.test",
                     size = 4.5,
                     symnum.args = list(cutpoints = c(0,  0.001, 0.01, 0.05, 1), 
                                        symbols = c("***", "**", "*", "ns")))#??????ʾp-value??λ??

p3

test$mortstat <- factor(test$mortstat, 
                         levels=c(0,1),
                         labels=c('Alive','Death'))
my_comparisons = list(c('Alive','Death'))
colors2 <- c('#1F77B4FF','#BD6263')

p4 <- ggplot(test, aes(x=mortstat,y=Zinc,fill=mortstat)) + 
  labs(title = 'Zinc')+xlab('')+ylab('(mg)')+
  scale_y_continuous(limits = c(0, 65),expand = c(0,0))+
  sm_raincloud(boxplot.params=list(outlier.shape = NA),
               point.params = list(size = 2,color='gray')) +
  scale_fill_manual(values = colors2)+
  #scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(size=12,face = 'bold'), 
        axis.text.y = element_text(size=12,face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        strip.text.x = element_text(size = 12, face = 'bold'),
        strip.text.y = element_text(size = 12, face = 'bold'),
        legend.position='none',#ȥͼ??????ggplot???÷???ͬ
        plot.title = element_text(hjust = 0.5,size=12))+
  stat_compare_means(comparisons = my_comparisons, #?????Զ????ļ?????��֮????��?Ե?p-value
                     label.x=1,label.y=57,method = "t.test",
                     size = 4.5,
                     symnum.args = list(cutpoints = c(0,  0.001, 0.01, 0.05, 1), 
                                        symbols = c("***", "**", "*", "ns")))#??????ʾp-value??λ??

p4

p5 <- ggplot(test, aes(x=mortstat,y=Se,fill=mortstat)) + 
  labs(title = 'Selenium')+xlab('')+ylab('(mcg)')+
  scale_y_continuous(limits = c(0, 690),expand = c(0,0))+
  sm_raincloud(boxplot.params=list(outlier.shape = NA),
               point.params = list(size = 2,color='gray')) +
  scale_fill_manual(values = colors2)+
  #scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(size=12,face = 'bold'), 
        axis.text.y = element_text(size=12,face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        strip.text.x = element_text(size = 12, face = 'bold'),
        strip.text.y = element_text(size = 12, face = 'bold'),
        legend.position='none',#ȥͼ??????ggplot???÷???ͬ
        plot.title = element_text(hjust = 0.5,size=12))+
  stat_compare_means(comparisons = my_comparisons, #?????Զ????ļ?????��֮????��?Ե?p-value
                     label.x=1,label.y=610,method = "t.test",
                     size = 4.5,
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("***", "**", "*", "ns")))#??????ʾp-value??λ??

p5

p6 <- ggplot(test, aes(x=mortstat,y=Carotenoids,fill=mortstat)) + 
  labs(title = 'Carotenoids')+xlab('')+ylab('(mcg)')+
  scale_y_continuous(limits = c(0, 110000),expand = c(0,0))+
  sm_raincloud(boxplot.params=list(outlier.shape = NA),
               point.params = list(size = 2,color='gray')) +
  scale_fill_manual(values = colors2)+
  #scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(size=12,face = 'bold'), 
        axis.text.y = element_text(size=12,face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        strip.text.x = element_text(size = 12, face = 'bold'),
        strip.text.y = element_text(size = 12, face = 'bold'),
        legend.position='none',#ȥͼ??????ggplot???÷???ͬ
        plot.title = element_text(hjust = 0.5,size=12))+
  stat_compare_means(comparisons = my_comparisons, #?????Զ????ļ?????��֮????��?Ե?p-value
                     label.x=1,label.y=95000,method = "t.test",
                     size = 4.5,
                     symnum.args = list(cutpoints = c(0,  0.001, 0.01, 0.05, 1), 
                                        symbols = c( "***", "**", "*", "ns")))#??????ʾp-value??λ??

p6

tiff("output/three_variables.tiff", height = 15, width = 20, units = 'cm', 
     compression = "lzw", res = 300)
ggarrange(p1,p2,p3,p4,p5,p6,nrow=2,ncol=3,
          labels = c('A',"",'','B','',''),
          widths=c(3,3,3,3,3,3),heights = c(2,2,2,2,2,2))
dev.off()
