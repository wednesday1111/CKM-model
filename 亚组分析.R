rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(pROC)

auc <- read.csv("train_auc.csv")
train <- read.csv("train20250224.csv")
full<-cbind(train,auc)
#auc <- read.csv("test_auc.csv")
#test <- read.csv("test20250224.csv")
#full<-cbind(test,auc)

#sensitivity#######
#train
zs<-full[full$SEQN <90000,]
zs<-full[1202:nrow(full),]
#test
#zs<-full[full$SEQN <90000,]
#zs<-full[276:nrow(full),]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')

#subgroup##########
#age<60
zs<-full[full$Age<60,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#age>=60
zs<-full[full$Age>=60,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#gender1
zs<-full[full$Gender==2,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#CKM
zs<-full[full$CKM==4,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Smoke.status
zs<-full[full$Smoke.status==3,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Alcohol.status
zs<-full[full$Alcohol.status==3,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Cancer
zs<-full[full$Cancer==0,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Diabetes
zs<-full[full$Diabetes==1,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Hypertriglyceridemia
zs<-full[full$Hypertriglyceridemia==0,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Hypertension
zs<-full[full$Hypertension==1,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Physical.activity
zs<-full[full$Physical.activity==0,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Race
zs<-full[full$Race==4,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#FPIR
zs<-full[full$FPIR==1,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#Clinical.CVD
zs<-full[full$Clinical.CVD==1,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
#MetS
zs<-full[full$MetS==0,]
roc_1<-roc(zs$True_label,zs$RF_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$RF_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
roc_1<-roc(zs$True_label,zs$GBM_Pred,ci=T)
roc_1
roc_2<-roc(zs$True_label,zs$GBM_hybrid_Pred,ci=T)
roc_2
roc.test(roc_1,roc_2,method = 'delong')
