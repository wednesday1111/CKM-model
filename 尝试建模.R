rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library("randomForest")
library(survival)
#library(survminer)

data<-read.csv('full_data20250220.csv')
colnames(data)
data<-data[data$CKM!='stage 0',]
data<-data[,-c(1,17:20)]

data$mortstat<-factor(data$mortstat)
data$mortstat<-as.numeric(data$mortstat)
model_cox <- coxph(Surv(permth_int, mortstat) ~., data)
summary(data)
table(data$Smoke.status)



