rm(list = ls())
getwd()
setwd('/Volumes/T7/CKM')
library(forestploter)
library(readxl)
library(grid)

df <- read_excel('20250225/AUC.xlsx')
df$`AUC (95% CI)` <- ifelse(is.na(df$`AUC (95% CI)`), "", df$`AUC (95% CI)`)
df$auc <- ifelse(is.na(df$auc), "", df$auc)
df$lower <- ifelse(is.na(df$lower), "", df$lower)
df$upper <- ifelse(is.na(df$upper), "", df$upper)
df$`NRI (95% CI)` <- ifelse(is.na(df$`NRI (95% CI)`), "", df$`NRI (95% CI)`)
df$`IDI (95% CI)` <- ifelse(is.na(df$`IDI (95% CI)`), "", df$`IDI (95% CI)`)

df$n<-NA
df$n <- ifelse(is.na(df$n), "", df$n)

df$auc <- as.numeric(df$auc)
df$lower <- as.numeric(df$lower)
df$upper <- as.numeric(df$upper)

colnames(df)[8]<-'                          '

tm <- forest_theme(base_size = 14, 
                   core=list(bg_params=list(fill = c("white", "white", "white"))),
                   ci_pch = 20,
                   ci_col = "#4575b4", # #762a83
                   ci_lty = 1,
                   ci_lwd = 4,
                   ci_Theight = 0, 
                   
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")

g <- forest(df[,c(1,2,8,6,7)],
            est= df$auc,
            lower = df$lower,
            upper = df$upper,
            ci_column = 3,
            ref_line = 0.7,
            xlim = c(0.6,1),ticks_at = c(0.6,0.7,0.8,0.9,1),
            theme = tm
)
g
g <- edit_plot(g, col = 3, row = c(3,5,7,9,12,14,16,18), which = "ci",
               gp = gpar(col = "#BD6263", fill = "#BD6263"))
g
g <- add_border(g,
                part = "header",
                row = 1,
                gp = gpar(lwd = 1.5))
g
g <- add_border(g,
              part = "header",
              row = 10,
              gp = gpar(lwd = 0.5))
g
g <- edit_plot(g,
               row = c(1, 10),
               gp = gpar(fontface = "bold"))
g
g <- edit_plot(g, row = c(2,4,6,8,11,13,15,17), which = "background",
               gp = gpar(fill = "grey90"))
g

tiff(filename = "output/AUC.tiff", 
     width = 11,height = 6,units = "in",res = 300)
g
dev.off()   

