rm(list = ls())
#https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/stat-learn-lasso.html#statl-hitters-ridge-cv
getwd()
setwd('/Volumes/T7/CKM')

data<-read.csv('train20250224.csv')
colnames(data)
data<-data[,-c(1,17,68,69)]

#共线性诊断######
library(car)
VIF<-data.frame(vif(glm(mortstat~.,data=data,
        family = binomial(link = "logit"))))
colnames(VIF)<-'coefficient'
VIF$variable<-rownames(VIF)
VIF<-VIF[VIF$coefficient<10,]
select_col<-c(VIF$variable,'mortstat')
data <- data[,colnames(data)%in%select_col]

library(dplyr)
library(Hmisc)
library(corrplot)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(row = rownames(cormat)[row(cormat)[ut]], 
             column = rownames(cormat)[col(cormat)[ut]],cor =(cormat)[ut], p = pmat[ut])
}

res3 <- rcorr(as.matrix(data[,-29]),type='pearson')
res3 <-  flattenCorrMatrix(res3$r, res3$P)
delete <- res3[res3$cor>=0.7,]
delete_col<-c('Clinical.CVD')
data <- data[,!colnames(data)%in%delete_col]
#
data[,21:31]<-scale(data[,21:31], center = TRUE, scale = TRUE)
write.csv(data,"delete_data20250220.csv",row.names = FALSE)

#step P value#####
lm.full <- glm(mortstat~.,data=data,
               family = binomial(link = "logit"))
step_p<-stats::step(lm.full)
summary(step_p)
glm_p<-glm(mortstat ~ Gender + Age + Race + Cancer + Overweight.obesity + 
             Prediabetes + Diabetes + Hypertriglyceridemia + 
             Education + Marital.status + FPIR + Smoke.status + Alcohol.status + 
             Physical.activity + Zinc + 
             Se + Carotenoids,
         data=data,family = binomial(link = "logit"))
summary(glm_p)
step_P<-data.frame(glm_p$coefficients)
step_P<-rownames(step_P)[2:nrow(step_P)]

#step AIC#####
library(MASS) 
stepaic<-stepAIC(glm(mortstat~.,data=data,
                     family = binomial(link = "logit")))
summary(stepaic)
glm<-glm(mortstat ~ Gender + Age + Race + Cancer + Overweight.obesity + 
           Prediabetes + Diabetes + Hypertriglyceridemia + 
           Education + Marital.status + FPIR + Smoke.status + Alcohol.status + 
           Physical.activity + Zinc + 
           Se + Carotenoids,
         data=data,family = binomial(link = "logit"))
summary(glm)

stepAIC<-data.frame(glm$coefficients)
stepAIC<-rownames(stepAIC)[2:nrow(stepAIC)]

#reg subsets####
library(leaps)
regfit.full <- regsubsets(
  mortstat ~ ., data, nvmax=18)
reg.summary <- summary(regfit.full)
reg.summary
plot(reg.summary$bic)
coef(regfit.full, id=7)

#ridge regression#####
set.seed(1)
colnames(data)
x <- model.matrix(mortstat ~ ., data)[,-1]
y<-data$mortstat
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)
dim(coef(ridge.mod))
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(
  ridge.mod, s = bestlam, 
  newx =x)
mean( (ridge.pred - y)^2 ) |> sqrt()#0.3310555
ridge_coef <- coef(cv.out, s = "lambda.min")
ridge <- rownames(ridge_coef)[ridge_coef[, 1] != 0]
ridge <- lasso[2:length(ridge)]

#LASSO#####
library(glmnet)
set.seed(1)
colnames(data)
x <- model.matrix(mortstat ~ ., data)[,-1]
y<-data$mortstat
grid <- 10^seq(10, -2, length=100)
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid)
plot(lasso.mod)
cv.out <- cv.glmnet(x, y, alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min; bestlam#0.0005334123
lasso.pred <- predict(
  lasso.mod, s = bestlam, 
  newx = x)
mean( (lasso.pred - y)^2 ) |> sqrt()#0.3303204
lasso_coef <- coef(cv.out, s = "lambda.min")
lasso <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
lasso <- lasso[2:length(lasso)]

#adaptive lasso#####
library(msqps)
cv.out <- adaptive_lasso(y,x,alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
adape_lasso <- adaptive_lasso(y,x,alpha=1,lambda=max.iteration=100)
adape_lasso_variable<-adape_lasso

#Boruta#####
library(Boruta)
set.seed(2)
colnames(data)
x <- data[,-33]
y<-data$mortstat
boruta <- Boruta(mortstat~.,data=data, 
                 pValue=0.0001, mcAdj=T, 
                 maxRuns=100)
boruta
print(boruta)
boruta$ImpHistory
table(boruta$finalDecision)
boruta$finalDecision[boruta$finalDecision=="Confirmed"]
plot(boruta)

library(tidyr)
library(dplyr)
boruta_history <- as.data.frame(boruta$ImpHistory)
boruta_score <- gather(boruta_history)
boruta_score <- filter(boruta_score, value != -Inf)
length(unique(boruta_score$key))
colnames(boruta_score) <- c('features','value')

boruta_finalDecision <- as.data.frame(boruta$finalDecision)
allele <- rownames(boruta_finalDecision)
boruta_finalDecision <- cbind(allele,boruta_finalDecision)
colnames(boruta_finalDecision) <- c('features','rank')

final_data <- full_join(boruta_score,boruta_finalDecision,by='features')
table(final_data$rank)

final_data <- dplyr::mutate_at(final_data, .vars = vars(3), 
                               .fun = function(x) ifelse(is.na(x), 'Shadow value', x))
table(final_data$rank)

write.csv(final_data, file="output/Boruta.csv")


final_data$rank[which(final_data$rank ==1)] <- 'Tentative features'
final_data$rank[which(final_data$rank ==2)] <- 'Confirmed features'
final_data$rank[which(final_data$rank ==3)] <- 'Rejected features'
table(final_data$rank)

#MRMR#####
library(mRMRe)
mrmr_feature<-data
target_indices = which(names(data)=='mortstat')
for (m in which(sapply(mrmr_feature, class)!="numeric")){
  mrmr_feature[,m]=as.numeric(mrmr_feature[,m])
}
Data <- mRMR.data(data = data.frame(mrmr_feature))
mrmr=mRMR.ensemble(data = Data, target_indices = target_indices, 
                   feature_count = 20, solution_count = 1)

#获取筛选出来的特征的列，包含在mrmr@filters中，mrmr@filters[原特征个数]这个list里
index=mrmr@filters[[as.character(mrmr@target_indices)]]
#获取训练集特征
train_feature <- mrmr_feature[,index]
print(colnames(train_feature))
mrmr_feature<-colnames(train_feature)
#获取相关性评分（Relevance）
relevance_scores <- scores(mrmr)
relevance_scores
#计算冗余评分（Redundancy）
redundancy_matrix <- mim(mrmr)
selected_features <- featureNames(mrmr)[index]
redundancy_scores <- sapply(seq_along(selected_features), function(i) {
  if (i == 1) {
    0  # 第一个特征无冗余
  } else {
    mean(redundancy_matrix[selected_features[i], selected_features[1:(i-1)]])
  }
})
result <- data.frame(
  Feature = featureNames(Data)[index],
  Relevance = scores(mrmr),
  Redundancy = redundancy_scores
)
print(result)
#upset#######





