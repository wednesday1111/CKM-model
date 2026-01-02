rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')

data<-read.csv('train.csv')
colnames(data)
data<-data[,-c(1,17,68,69)]

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

#Boruta#####
library(Boruta)
set.seed(2)
colnames(data)
x <- data[,-33]
y<-data$mortstat
boruta <- Boruta(mortstat~.,data=data, 
                 pValue=0.0001, mcAdj=T, 
                 maxRuns=1000)
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

index=mrmr@filters[[as.character(mrmr@target_indices)]]
train_feature <- mrmr_feature[,index]
print(colnames(train_feature))
mrmr_feature<-colnames(train_feature)
#Relevance
relevance_scores <- scores(mrmr)
relevance_scores
#Redundancy
redundancy_matrix <- mim(mrmr)
selected_features <- featureNames(mrmr)[index]
redundancy_scores <- sapply(seq_along(selected_features), function(i) {
  if (i == 1) {
    0  
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





