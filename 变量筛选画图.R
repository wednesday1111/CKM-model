rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(UpSetR)

# Example data for three sets
set1 <- c("Gender","Age","Race","Cancer","Overweight.obesity","Prediabetes",         
          "Diabetes","Hypertriglyceridemia","Education","Marital.status", 
          "FPIR","Smoke.status","Alcohol.status","Physical.activity",
          "Zinc","Se","Carotenoids")
set2 <- c("Pelargonidin","Prediabetes","Peonidin","Cancer","Vitamin.E",
          "Se", "Eriodictyol","CKM","Isorhamnetin","Carotenoids",
          "Hypertriglyceridemia","Race","Overweight.obesity","Zinc",
          "Alcohol.status","Physical.activity","Vitamin.C","CKD",           
          "Gender","Age")
set3 <- c("Gender","Se",'Zinc','Mg','Vitamin.E','Vitamin.C',
          'Isorhamnetin','Eriodictyol','Peonidin','Pelargonidin',
          'Physical.activity','Alcohol.status','Smoke.status',
          'Marital.status','CKM','Hypertriglyceridemia',
          'Prediabetes','Abdominal.obesity','Overweight.obesity',
          'CVD.risk','Race','Age','Carotenoids')
max_len<-length(set3)
df<-data.frame(
  Stepwise=c(set1, rep(NA, max_len - length(set1))),
  MRMR=c(set2, rep(NA, max_len - length(set2))),
  Boruta=c(set3, rep(NA, max_len - length(set3)))
)
#plot######
p1<-upset(fromList(df), 
      order.by = "freq", 
      matrix.color = '#4575b4',
      main.bar.color = '#4575b4',
      sets.bar.color = '#374E55FF',
      queries = list(
        list(
          query = intersects, 
          params = list("Stepwise", "MRMR","Boruta"), 
          color = "#BD6263", 
          active = T)),
      point.size = 4, # 点大小
      line.size = 1.5,# 线粗细
      mb.ratio = c(0.7, 0.3),
      text.scale = 2,
      mainbar.y.label = "Count of Intersection",  # y 标题
      sets.x.label = "Number of Variables"
)
p1

library(gridExtra)
gridtext::textbox_grob("This is a text box", x = 0.7, y = 0.85, gp = gpar(fontsize = 15, col = "red"))

tiff("output/variable_selection.tiff",width = 2900,height = 2000,units = "px",
     res=300)
p1
dev.off()
#variable selection######
var1<-intersect(set1,set2)
var2<-intersect(var1,set3)
print(var2)


      