# Working path ------------------------------------------------------------
rm(list = ls())
setwd("F:\\Desktop\\TEST1")


# Package -----------------------------------------------------------------
library(dplyr) #filter函数进行数据整理
library(agricolae) #显著性差异检验
library(ggplot2) #画图


# Data generation ---------------------------------------------------------
df_row <- data.frame(Time = rep(c(12, 24, 36, 48, 60), each = 80),
                     value = c(rnorm(80, mean = 4.5, sd = 0.5),
                               rnorm(80, mean = 8, sd = 1.5),
                               rnorm(80, mean = 9, sd = 1.8),
                               rnorm(80, mean = 12,sd = 2),
                               rnorm(80, mean = 16, sd = 3)),
                     treat = rep(c('CK','Treat-1','Treat-2','Treat-3'),
                                 each = 20, times = 5),
                     phase = rep(c('Stage_1', 'Stage_2'),
                                 each = 5, times = 40),
                     duration = rep(c('Short','Long'),
                                    each = 10, times = 20))
head(df_row)
#write.csv(df_row, 'df_row.csv')

# Data analysis -----------------------------------------------------------
Time <- unique(df_row$Time)
Duration <- unique(df_row$duration)
Phase <- unique(df_row$phase)

result_summary <- data.frame()
current_result <- data.frame()


for (t in Time){
  df1 <- filter(df_row, Time == t)
  for (d in Duration) {
    df2 <- filter(df1, duration == d)
    for (p in Phase) {
      df3 <- filter(df2, phase == p)
      mod <- aov(value ~ treat, data = df3)
      hsd <- HSD.test(mod, "treat")
      current_result <- hsd$groups
      current_result$treat <- row.names(current_result)
      current_result$duration <- d
      current_result$time <- t
      current_result$phase <- p
      current_result$std <- hsd$means$std
      result_summary <- rbind(result_summary, current_result)
      row.names(result_summary) <- c(1:nrow(result_summary))
    }
  }
}
result_summary$time <- factor(result_summary$time,
                              levels = c(12,24,36,48,60))
#write.csv(result_summary,"result_summary.csv") #输出生成的表格数据


# Plot --------------------------------------------------------------------
ggplot(result_summary, aes(x=time, y=value, fill = treat, color = treat))+
  facet_grid(duration~phase)+
  geom_bar(stat = "identity", position = "dodge", width = 0.75)+
  geom_errorbar(aes(ymin=value-std, ymax=value+std), 
                position = position_dodge(width = 0.75), width= 0.5, size=0.2)+
  scale_y_continuous(limits = c(0,22), breaks = seq(0,21,5), expand = c(0,0))+
  scale_fill_manual(values = c("#afc978","#a9dcf2","#edc0c0","#ddaaf7"))+
  scale_color_manual(values = c('#6a8c17','#106c8c','#d87d7d','#823aad'))+
  geom_text(aes(x= time, y = value, label = groups), position = position_dodge(width = 0.75),
            vjust=-1.5, color="black", size = 3)+
  theme_bw()+
  labs(x = "Operating time (day)",
       y = "Dry weight (g)")

ggsave("plot.pdf", width = 7, height = 3.6)