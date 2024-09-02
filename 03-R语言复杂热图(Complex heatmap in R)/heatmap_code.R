# Path --------------------------------------------------------------------
setwd("F:\\Desktop\\03-R语言复杂热图(Complex heatmap in R)")
rm(list = ls())

# Data --------------------------------------------------------------------
df_row <- read.csv("ASV_RA.csv", row.names = 1)
df1 <- as.data.frame(scale(df_row, center = TRUE, scale = TRUE))
env_df <- data.frame(row.names = row.names(t(df_row)))
for (i in 1:8) {
  env_df[paste0('index',i)] <- c(runif(15, 0, 1.5),runif(15, 2, 5))
}
# pheatmap ----------------------------------------------------------------
#install.packages("pheatmap")
library(pheatmap)

group <- sort(colnames(df1), decreasing = F)
group
col <- data.frame(row.names = group,
                  Treat = rep(c('Treat1','Treat2'), each = 15),
                  Time = rep(c("2022","2023","2024"), each = 5, times = 2))

ann_color <- list(
  Treat = c("Treat1" = "#fce4b8","Treat2" = "#c9def4"),
  Time = c("2022" = "#eeeeee", "2023" = "#b7b7b7", "2024" = "#707070"))

pheatmap(df1, scale = "row",
         annotation_col = col,
         cluster_rows = TRUE,
         cutree_rows = 3,
         cluster_cols = TRUE, treeheight_col = 15,
         cutree_cols = 2,
         border_color = "white",
         annotation_colors = ann_color,
         angle_col = 315,
         color = colorRampPalette(c("#ffffff","#d0e2f2","#388fc5", "#1f3a73"))(100)
)

# ComplexHeatmap ----------------------------------------------------------
library(ComplexHeatmap)
library(circlize)
df2 <- as.matrix(df1)

number <- runif(43, 10,30)
number_df <- data.frame(ID = row.names(df2),
                           count = number)

row_ha = rowAnnotation(Count = anno_barplot(number))
col_fun = colorRamp2(c(-0.3, -0.05, 1), c("#016500","white","#faa910" ))
split_col = rep(1:2, each =15)
split_col
split_row = c(rep(1:2, each = 15),rep(3, each=13))
split_row

Heatmap(df2, col = col_fun,
        cluster_rows = T,
        cluster_columns = T,
        rect_gp = gpar(col = "white", lwd = 1),
        column_names_rot = -60,
        left_annotation = row_ha,
        border_gp = gpar(col = "black", lty = 2),
        row_names_side = "left",
        column_split = split_col,
        row_split = split_row,
        top_annotation = HeatmapAnnotation(foo = anno_block(
          gp = gpar(fill = c("#fed0d0","#fc9599")),
          labels = c("Treat1", "Treat2"),
          labels_gp = gpar(col = "white", fontsize = 10))))

# Correlation -------------------------------------------------------------
library(psych)
library(ggplot2)

res <- corr.test(t(df1), env_df, use = 'pairwise', 
                 method = 'spearman', adjust = 'holm',
                 alpha = 0.05)        

res$p.adj
res$r
write.table(res$p.adj,"pvalue.csv", sep = ',')
write.table(res$r,"rvalue.csv", sep = ',')
pdat <- res$p.adj

### ---- Plot1 ---
pdat_df <- data.frame(ASV = rep(rownames(pdat), 8),
                    index = rep(colnames(pdat), each = 43),
                    r = as.vector(res$r),
                    p = as.vector(res$p.adj),
                    r_f = runif(344,-1,1),
                    p_f = runif(344,0,0.8))

pdat_df$p_mark <- as.character(symnum(pdat_df$p_f, 
                                              cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                              symbols = c("***", "**", "*", "")))

ggplot(pdat_df, aes(ASV, index,  size = -log10(p_f))) +
  geom_point(aes(alpha = -log10(p_f), fill = r_f), shape = 21, color = "#999999") + 
  scale_fill_gradient2(low = '#7e5ae8',
                       mid = 'white',
                       high = '#ff5e3a',
                       midpoint = 0) +  #midpoit中间值是几
  scale_size_area() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  geom_text(aes(label=p_mark), color="#39434b", size = 3)+
  xlab("")+
  ylab("")
  #coord_flip() #横纵坐标进行翻转

### --- Plot2 ---
library(dplyr)
library(reshape)
myfun <- function(pval) {
  stars = ""
  if(pval <= 0.001)
    stars = "***"
  if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  if(pval > 0.05 & pval <= 0.1)
    stars = ""
  stars
}

heatmap_df <- melt(res$r) %>% rename(replace=c("X1"="ASV","X2"="index",
                                          "value"="r")) %>%
  mutate(p=melt(res$p.adj)[,3]) %>%
  mutate(p_f = pdat_df$p_f) %>%
  mutate(r_f = pdat_df$r_f) %>%
  mutate(p_mark = sapply(p_f, function(x) myfun(x)))

ggplot(heatmap_df,aes(ASV,index,col=r_f))+
  geom_tile(color="grey70",fill="white",size=1)+
  geom_point(aes(size = abs(r_f)),shape=15) +
  geom_text(aes(label=p_mark),size=6,color="white",
            hjust=0.5,vjust=0.7)+
  labs(x = NULL, y = NULL,color=NULL)+
  scale_color_gradient2(low = "#dd8a0b",
                        mid = "grey92", high = "#32a676")+
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_rect(fill=NA,color="grey70",
                                    size=2, linetype="solid")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size(range=c(1,10),guide=NULL)+
  guides(color = guide_colorbar(direction = "vertical",reverse = F, 
                                barwidth = unit(.5, "cm"),
                                barheight = unit(15, "cm")))
ggsave("Rplot_4.pdf", width = 14, height = 3.3)
