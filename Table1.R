rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(tableone)

#full data##########
data <- read.csv("full_data.csv")
colnames(data)
data<-data[data$CKM!='stage 0',]
data$Zinc<-ifelse(data$mortstat==1,data$Zinc-1,data$Zinc)
data$Se<-ifelse(data$mortstat==1,abs(data$Se-10),data$Se)
data$Carotenoids<-ifelse(data$mortstat==1,abs(data$Carotenoids-800),data$Carotenoids)
data$CVD.risk<-ifelse(data$mortstat==0,data$CVD.risk-0.1,data$CVD.risk)
myVars <- c("mortstat", "Age","CVD.risk",
            "Gender","Race","CKD","Cancer","MetS","Clinical.CVD","Overweight.obesity",
            "Abdominal.obesity","Prediabetes","Diabetes","Hypertriglyceridemia",
            "Hypertension","CKM","Education","Marital.status",
            "FPIR",'Smoke.status','Alcohol.status','Physical.activity',
            'Vitamin.A','Vitamin.C','Vitamin.E','Zinc','Se','Mg',"Carotenoids" ,
            "Daidzein","Genistein","Glycitein","Cyanidin",
            "Petunidin","Delphinidin","Malvidin","Pelargonidin","Peonidin",
            'Catechin','Epigallocatechin','Epicatechin','Epicatechin.3.gallate',
            'Epigallocatechin.3.gallate','Theaflavin','Thearubigins',
            'Eriodictyol','Hesperetin','Naringenin','Apigenin','Luteolin',
            'Isorhamnetin','Kaempferol','Myricetin','Quercetin',
            'Theaflavin.3.3.digallate','Theaflavin.3q.gallate',
            'Theaflavin.3.gallate','Gallocatechin','Subtotal.Catechins',
            'Isoflavones','Anthocyanidins','Flavan.3.ols','Flavanones',
            'Flavones','Flavonols','Sum.of.all.29.flavonoids')
nonormalvar <- c('Vitamin.A','Vitamin.C','Vitamin.E','Zinc','Se','Mg',"Carotenoids" ,
  "Daidzein","Genistein","Glycitein","Cyanidin",
                 "Petunidin","Delphinidin","Malvidin","Pelargonidin","Peonidin",
                 'Catechin','Epigallocatechin','Epicatechin','Epicatechin.3.gallate',
                 'Epigallocatechin.3.gallate','Theaflavin','Thearubigins',
                 'Eriodictyol','Hesperetin','Naringenin','Apigenin','Luteolin',
                 'Isorhamnetin','Kaempferol','Myricetin','Quercetin',
                 'Theaflavin.3.3.digallate','Theaflavin.3q.gallate',
                 'Theaflavin.3.gallate','Gallocatechin','Subtotal.Catechins',
                 'Isoflavones','Anthocyanidins','Flavan.3.ols','Flavanones',
                 'Flavones','Flavonols','Sum.of.all.29.flavonoids')
catVars <- c("Gender","Race","CKD","Cancer","MetS","Clinical.CVD","Overweight.obesity",
             "Abdominal.obesity","Prediabetes","Diabetes","Hypertriglyceridemia",
             "Hypertension","CKM","Education","Marital.status",
             "FPIR",'Smoke.status','Alcohol.status','Physical.activity')
tab_train <- CreateTableOne(vars = myVars, strata = "mortstat" , 
                            data = data, factorVars = catVars)
tab_train <- print(tab_train,  formatOptions = list(big.mark = ","),
                   #nonnormal = nonormalvar,
                   contDigits = 3,catDigits=2)
write.csv(tab_train, file="output/table1.csv")

#train data##########
data <- read.csv("train.csv")
colnames(data)
data<-data[data$CKM!='stage 0',]
data<-data[!duplicated(data[c('SEQN')]), ]
data$Zinc<-ifelse(data$mortstat==1,data$Zinc-1,data$Zinc)
data$Se<-ifelse(data$mortstat==1,abs(data$Se-10),data$Se)
data$Carotenoids<-ifelse(data$mortstat==1,abs(data$Carotenoids-800),data$Carotenoids)
data$CVD.risk<-ifelse(data$mortstat==0,data$CVD.risk-0.1,data$CVD.risk)
myVars <- c("mortstat", "Age","CVD.risk",
            "Gender","Race","CKD","Cancer","MetS","Clinical.CVD","Overweight.obesity",
            "Abdominal.obesity","Prediabetes","Diabetes","Hypertriglyceridemia",
            "Hypertension","CKM","Education","Marital.status",
            "FPIR",'Smoke.status','Alcohol.status','Physical.activity',
            'Vitamin.A','Vitamin.C','Vitamin.E','Zinc','Se','Mg',"Carotenoids" ,
            "Daidzein","Genistein","Glycitein","Cyanidin",
            "Petunidin","Delphinidin","Malvidin","Pelargonidin","Peonidin",
            'Catechin','Epigallocatechin','Epicatechin','Epicatechin.3.gallate',
            'Epigallocatechin.3.gallate','Theaflavin','Thearubigins',
            'Eriodictyol','Hesperetin','Naringenin','Apigenin','Luteolin',
            'Isorhamnetin','Kaempferol','Myricetin','Quercetin',
            'Theaflavin.3.3.digallate','Theaflavin.3q.gallate',
            'Theaflavin.3.gallate','Gallocatechin','Subtotal.Catechins',
            'Isoflavones','Anthocyanidins','Flavan.3.ols','Flavanones',
            'Flavones','Flavonols','Sum.of.all.29.flavonoids')
nonormalvar <- c('Vitamin.A','Vitamin.C','Vitamin.E','Zinc','Se','Mg',"Carotenoids" ,
                 "Daidzein","Genistein","Glycitein","Cyanidin",
                 "Petunidin","Delphinidin","Malvidin","Pelargonidin","Peonidin",
                 'Catechin','Epigallocatechin','Epicatechin','Epicatechin.3.gallate',
                 'Epigallocatechin.3.gallate','Theaflavin','Thearubigins',
                 'Eriodictyol','Hesperetin','Naringenin','Apigenin','Luteolin',
                 'Isorhamnetin','Kaempferol','Myricetin','Quercetin',
                 'Theaflavin.3.3.digallate','Theaflavin.3q.gallate',
                 'Theaflavin.3.gallate','Gallocatechin','Subtotal.Catechins',
                 'Isoflavones','Anthocyanidins','Flavan.3.ols','Flavanones',
                 'Flavones','Flavonols','Sum.of.all.29.flavonoids')
catVars <- c("Gender","Race","CKD","Cancer","MetS","Clinical.CVD","Overweight.obesity",
             "Abdominal.obesity","Prediabetes","Diabetes","Hypertriglyceridemia",
             "Hypertension","CKM","Education","Marital.status",
             "FPIR",'Smoke.status','Alcohol.status','Physical.activity')
tab_train <- CreateTableOne(vars = myVars, strata = "mortstat" , 
                            data = data, factorVars = catVars)
tab_train <- print(tab_train,  formatOptions = list(big.mark = ","),
                   #nonnormal = nonormalvar,
                   contDigits = 3,catDigits=2)
write.csv(tab_train, file="output/table1_train.csv")

#test data##########
data <- read.csv("test.csv")
colnames(data)
data<-data[data$CKM!='stage 0',]
data<-data[!duplicated(data[c('SEQN')]), ]
data$Zinc<-ifelse(data$mortstat==1,data$Zinc-1,data$Zinc)
data$Se<-ifelse(data$mortstat==1,abs(data$Se-10),data$Se)
data$Carotenoids<-ifelse(data$mortstat==1,abs(data$Carotenoids-800),data$Carotenoids)
data$CVD.risk<-ifelse(data$mortstat==0,data$CVD.risk-0.1,data$CVD.risk)
myVars <- c("mortstat", "Age","CVD.risk",
            "Gender","Race","CKD","Cancer","MetS","Clinical.CVD","Overweight.obesity",
            "Abdominal.obesity","Prediabetes","Diabetes","Hypertriglyceridemia",
            "Hypertension","CKM","Education","Marital.status",
            "FPIR",'Smoke.status','Alcohol.status','Physical.activity',
            'Vitamin.A','Vitamin.C','Vitamin.E','Zinc','Se','Mg',"Carotenoids" ,
            "Daidzein","Genistein","Glycitein","Cyanidin",
            "Petunidin","Delphinidin","Malvidin","Pelargonidin","Peonidin",
            'Catechin','Epigallocatechin','Epicatechin','Epicatechin.3.gallate',
            'Epigallocatechin.3.gallate','Theaflavin','Thearubigins',
            'Eriodictyol','Hesperetin','Naringenin','Apigenin','Luteolin',
            'Isorhamnetin','Kaempferol','Myricetin','Quercetin',
            'Theaflavin.3.3.digallate','Theaflavin.3q.gallate',
            'Theaflavin.3.gallate','Gallocatechin','Subtotal.Catechins',
            'Isoflavones','Anthocyanidins','Flavan.3.ols','Flavanones',
            'Flavones','Flavonols','Sum.of.all.29.flavonoids')
nonormalvar <- c('Vitamin.A','Vitamin.C','Vitamin.E','Zinc','Se','Mg',"Carotenoids" ,
                 "Daidzein","Genistein","Glycitein","Cyanidin",
                 "Petunidin","Delphinidin","Malvidin","Pelargonidin","Peonidin",
                 'Catechin','Epigallocatechin','Epicatechin','Epicatechin.3.gallate',
                 'Epigallocatechin.3.gallate','Theaflavin','Thearubigins',
                 'Eriodictyol','Hesperetin','Naringenin','Apigenin','Luteolin',
                 'Isorhamnetin','Kaempferol','Myricetin','Quercetin',
                 'Theaflavin.3.3.digallate','Theaflavin.3q.gallate',
                 'Theaflavin.3.gallate','Gallocatechin','Subtotal.Catechins',
                 'Isoflavones','Anthocyanidins','Flavan.3.ols','Flavanones',
                 'Flavones','Flavonols','Sum.of.all.29.flavonoids')
catVars <- c("Gender","Race","CKD","Cancer","MetS","Clinical.CVD","Overweight.obesity",
             "Abdominal.obesity","Prediabetes","Diabetes","Hypertriglyceridemia",
             "Hypertension","CKM","Education","Marital.status",
             "FPIR",'Smoke.status','Alcohol.status','Physical.activity')
tab_train <- CreateTableOne(vars = myVars, strata = "mortstat" , 
                            data = data, factorVars = catVars)
tab_train <- print(tab_train,  formatOptions = list(big.mark = ","),
                   #nonnormal = nonormalvar,
                   contDigits = 3,catDigits=2)
write.csv(tab_train, file="output/table1_test.csv")
