setwd("F:/Desktop/volcano")
rm(list = ls())
 
library(ggplot2)
library(dplyr)
library(ggrepel)

df1 <- read.csv("df1.csv",row.names = 1)
df1 <- df1 %>% arrange(padj)
df1$lable <- c(rownames(df1)[1:25],rep(NA,nrow(df1)-25))
padj<- df1$padj
logpadj<- -log10(padj)


# plot1 -------------------------------------------------------------------
ggplot(df1, aes(log2, -log10(padj))) +
  geom_point(aes(size = -log10(padj),
                 color = -log10(padj))) + # 数据点
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed", color = "#999999") +
  geom_vline(xintercept = c(-1.2, 1.2),
             linetype = "dashed", color = "#999999") + #虚线
  scale_color_gradientn(values = seq(0, 1, 0.2),
                        colors = c("#39489f", "#39bbec",
                                   "#f9ed36", "#f38466", "#b81f25")) +
  scale_size_continuous(range = c(1, 5)) +
  labs(x = "Log2FC (Group1 vs. Group2)", 
       y = "-Log10(FDR q-value)") +
  guides(color = guide_colorbar(title = "-Log10_p-value"),
         size = guide_legend(title = ""),
         fill = guide_legend(title = "-Log10 q-value")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) + # x,y轴字体大小
  annotate("text", x = 5.8, y = 1.2,
           label = "q-value<0.05",
           color = "black",
           fontface = "italic") +
  annotate("text", x = -2, y = 3.8,
           label = "FC<1.2", color = "black") +
  annotate("text", x = 2, y = 3.8,
           label = "FC>1.2", color = "black") + # 添加标签
  geom_text_repel(aes(label = lable, color = -log10(padj)), 
                  size = 3, 
                  min.segment.length = 0.5, 
                  box.padding = 0.5,
                  max.overlaps = Inf,
                  arrow = arrow(length = unit(0.010, "npc")))
 
ggsave("vocanol_plot1.pdf", height = 6, width = 8)
  

# plot2 -------------------------------------------------------------------
df2 <- read.csv("df2.csv", row.names = 1)

df2$col[df2$signif==F] <- "gray60"
df2$col[df2$logFC>0 & df2$signif==T] <-"#ff0000"
df2$col[df2$logFC<0 & df2$signif==T] <-"#00ff00"

count <- table(df2$col[df2$col %in% c("gray60","#ff0000","#00ff00")])
up_count <- count["#ff0000"]
down_count <- count["#00ff00"]

ggplot(df2, aes(x = log2(CPM), y = logFC,
                color = factor(col), shape = factor(col))) +
  geom_point() +
  scale_shape_manual(values = c(16, 16, 1)) +
  scale_color_manual(values = c("#00ff00", "#ff0000", "gray60")) +
  scale_y_continuous(limits = c(-6, 6), 
                     breaks = seq(-5, 5, by = 5), 
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(6, 16), 
                     breaks = seq(6, 15, by = 3)) +
  labs(x = "Log2(Count per million)", 
       y = "Log2(Fold change)") +
  annotate("text", x = 6.5, y = 5.5,
           label = "UP:72", color = "black",
           fontface = "italic") +
  annotate("text", x = 6.5, y = -5.5,
           label = "DOWN:49", color = "black",
           fontface = "italic") +
  theme(legend.position = "none", 
        axis.text = element_text(size = rel(2)),
        axis.title = element_text(size = rel(2)))

ggsave("vocanol_plot2.pdf", height = 5.5, width = 6.5)





