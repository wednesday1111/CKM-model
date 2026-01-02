rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(survminer)
library(survival)
library(ggthemes)

auc <- read.csv("train_auc.csv")
train <- read.csv("train20250224.csv")
full<-cbind(train,auc)
full$stage <- cut(full$RF_hybrid_Pred, 
               breaks = c(-Inf, 0.5,Inf), 
               labels = c("Low", "High"), 
               right=TRUE)
full$permth_int
fit<-survfit(Surv(permth_int,mortstat)~stage,data=full)
summary(fit)$table
p1 <- ggsurvplot(fit, data = full,
                 #title="A", 
                 #fun = 'pct',
                 font.main = c(8, "plain", "Black"),
                 font.x = c(12, "bold", "Black"),
                 font.y = c(12, "bold", "Black"),
                 font.tickslab = c(12, "plain", "Black"),
                 pval = TRUE,pval.size = 4,pval.coord=c(10,0.2),
                 censor=FALSE, 
                 surv.scale="percent" ,
                 break.x.by = 30,
                 break.y.by = 0.25,
                 axes.offset = FALSE,
                 #xlim = c(0,30),
                 ylim = c(0,1),
                 xlab = "Follow-up (month)", 
                 legend = c(0.82,0.95), 
                 legend.title = "", 
                 legend.labs = c("Low risk",
                                 "High risk"), 
                 risk.table = FALSE,
                 #risk.table.fontsize = 3,
                 #tables.height = 0.2,
                 #tables.col = 'Black',
                 #tables.y.text.col = FALSE,
                 #tables.theme = theme_cleantable(), 
                 ggtheme = theme_few(),
                 #risk.table.col=FALSE,
                 palette = c('#1F77B4FF','#BD6263'))
p1<-p1$plot+theme(legend.background=element_rect(fill=rgb(1,1,1,alpha=0.001),colour=NA),
                  legend.text = element_text(size = 12))
p1

full$stage <- cut(full$GBM_hybrid_Pred, 
                  breaks = c(-Inf, 0.5,Inf), 
                  labels = c("Low", "High"), 
                  right=TRUE)
fit<-survfit(Surv(permth_int,mortstat)~stage,data=full)
summary(fit)$table
p3 <- ggsurvplot(fit, data = full,
                 #title="A", 
                 #fun = 'pct',
                 font.main = c(8, "plain", "Black"),
                 font.x = c(12, "bold", "Black"),
                 font.y = c(12, "bold", "Black"),
                 font.tickslab = c(12, "plain", "Black"),
                 pval = TRUE,pval.size = 4,pval.coord=c(10,0.2),
                 censor=FALSE, 
                 surv.scale="percent" ,
                 break.x.by = 30,
                 break.y.by = 0.25,
                 axes.offset = FALSE,
                 #xlim = c(0,30),
                 ylim = c(0,1),
                 xlab = "Follow-up (month)", 
                 legend = 'none', 
                 legend.title = "", 
                 legend.labs = c("Low risk",
                                 "High risk"), 
                 risk.table = FALSE,
                 #risk.table.fontsize = 3,
                 #tables.height = 0.2,
                 #tables.col = 'Black',
                 #tables.y.text.col = FALSE,
                 #tables.theme = theme_cleantable(), 
                 ggtheme = theme_few(),
                 #risk.table.col=FALSE,
                 palette = c('#1F77B4FF','#BD6263'))
p3

auc <- read.csv("test_auc.csv")
test <- read.csv("test20250224.csv")
full<-cbind(test,auc)
full$stage <- cut(full$RF_hybrid_Pred, 
                  breaks = c(-Inf, 0.5,Inf), 
                  labels = c("Low", "High"), 
                  right=TRUE)
fit<-survfit(Surv(permth_int,mortstat)~stage,data=full)
summary(fit)$table
p2 <- ggsurvplot(fit, data = full,
                 #title="A", 
                 #fun = 'pct',
                 font.main = c(8, "plain", "Black"),
                 font.x = c(12, "bold", "Black"),
                 font.y = c(12, "bold", "Black"),
                 font.tickslab = c(12, "plain", "Black"),
                 pval = TRUE,pval.size = 4,pval.coord=c(10,0.2),
                 censor=FALSE, 
                 surv.scale="percent" ,
                 break.x.by = 30,
                 break.y.by = 0.25,
                 axes.offset = FALSE,
                 #xlim = c(0,30),
                 ylim = c(0,1),
                 xlab = "Follow-up (month)", 
                 legend = 'none', 
                 legend.title = "", 
                 legend.labs = c("Low risk",
                                 "High risk"), 
                 risk.table = FALSE,
                 #risk.table.fontsize = 3,
                 #tables.height = 0.2,
                 #tables.col = 'Black',
                 #tables.y.text.col = FALSE,
                 #tables.theme = theme_cleantable(), 
                 ggtheme = theme_few(),
                 #risk.table.col=FALSE,
                 palette = c('#1F77B4FF','#BD6263'))
p2

full$stage <- cut(full$GBM_hybrid_Pred, 
                  breaks = c(-Inf, 0.5,Inf), 
                  labels = c("Low", "High"), 
                  right=TRUE)
fit<-survfit(Surv(permth_int,mortstat)~stage,data=full)
summary(fit)$table
p4 <- ggsurvplot(fit, data = full,
                 #title="A", 
                 #fun = 'pct',
                 font.main = c(8, "plain", "Black"),
                 font.x = c(12, "bold", "Black"),
                 font.y = c(12, "bold", "Black"),
                 font.tickslab = c(12, "plain", "Black"),
                 pval = TRUE,pval.size = 4,pval.coord=c(10,0.2),
                 censor=FALSE, 
                 surv.scale="percent" ,
                 break.x.by = 30,
                 break.y.by = 0.25,
                 axes.offset = FALSE,
                 #xlim = c(0,30),
                 ylim = c(0,1),
                 xlab = "Follow-up (month)", 
                 legend = 'none', 
                 legend.title = "", 
                 legend.labs = c("Low risk",
                                 "High risk"), 
                 risk.table = FALSE,
                 risk.table.fontsize = 4,
                 #tables.height = 0.2,
                 #tables.col = 'Black',
                 #tables.y.text.col = FALSE,
                 #tables.theme = theme_cleantable(), 
                 ggtheme = theme_few(),
                 #risk.table.col=FALSE,
                 palette = c('#1F77B4FF','#BD6263'))
p4

p <- ggpubr::ggarrange(p1, p2$plot, p3$plot, p4$plot, 
                       nrow = 2, ncol = 2, 
                        labels = c('A', 'B', 'C', 'D'), 
                        font.label = list(color = 'Black'))
p
ggsave(filename = "Output/KM.tif", device='tiff',width = 10,height = 7, 
       units = "in",dpi = 300)



