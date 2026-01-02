rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(PredictABEL) 
library(pROC)

#train <- read.csv("train_auc.csv")
#auc<-train
test <- read.csv("test_auc.csv")
auc<-test
roc_1<-roc(auc$True_label,auc$LR_Pred,ci=T)
roc_1$ci
roc_2<-roc(auc$True_label,auc$LR_hybrid_Pred,ci=T)
roc_2$ci
roc.test(roc_1,roc_2,method = 'delong')
pold<-auc$LR_Pred
pnew<-auc$LR_hybrid_Pred
reclassification(data = auc, cOutcome = 1, 
                 predrisk1 = pold,predrisk2 = pnew,cutoff = c(0,0.3,0.7,1))

roc_1<-roc(auc$True_label,auc$SVM_Pred,ci=T)
roc_1$ci
roc_2<-roc(auc$True_label,auc$SVM_hybrid_Pred,ci=T)
roc_2$ci
roc.test(roc_1,roc_2,method = 'delong')
pold<-auc$SVM_Pred
pnew<-auc$SVM_hybrid_Pred
reclassification(data = auc, cOutcome = 1, 
                 predrisk1 = pold,predrisk2 = pnew,cutoff = c(0,0.3,0.7,1))

roc_1<-roc(auc$True_label,auc$RF_Pred,ci=T)
roc_1$ci
roc_2<-roc(auc$True_label,auc$RF_hybrid_Pred,ci=T)
roc_2$ci
roc.test(roc_1,roc_2,method = 'delong')
pold<-auc$RF_Pred
pnew<-auc$RF_hybrid_Pred
reclassification(data = auc, cOutcome = 1, 
                 predrisk1 = pnew,predrisk2 = pold,cutoff = c(0,0.3,0.7,1))

roc_1<-roc(auc$True_label,auc$GBM_Pred,ci=T)
roc_1$ci
roc_2<-roc(auc$True_label,auc$GBM_hybrid_Pred,ci=T)
roc_2$ci
roc.test(roc_1,roc_2,method = 'delong')
pold<-auc$GBM_Pred
pnew<-auc$GBM_hybrid_Pred
reclassification(data = auc, cOutcome = 1, 
                 predrisk1 = pold,predrisk2 = pnew,cutoff = c(0,0.3,0.7,1))
