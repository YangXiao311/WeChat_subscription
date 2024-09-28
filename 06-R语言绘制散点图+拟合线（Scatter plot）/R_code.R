
# 设置工作路径 ------------------------------------------------------------------
setwd("F:\\Desktop\\t1")

# 加载所需包 --------------------------------------------------------------------
library(readxl)
library(ggplot2)

# 数据读入 --------------------------------------------------------------------
row_data <- as.matrix(read_xlsx("df.xlsx"))

# 数据处理 --------------------------------------------------------------------
data <- row_data[,2:ncol(row_data)]
treat <- c("E1-Cu", "E2-CuO", "E3-As", "E4-CM")
pressure <- seq(from = 2, to = 16, by = 2)
df <- data.frame(x = rep(pressure, each = 12),
                 y = as.vector(data),
                 treat = rep(rep(treat, each = 3), 8))
df$treat <- factor(df$treat, levels = c("E1-Cu", "E2-CuO", "E3-As", "E4-CM"))
df_mod <- transform(df,y = as.numeric(y))

# 画图 ----------------------------------------------------------------------
ggplot(df_mod, aes(x = x, y = y,
                   color = treat, fill = treat))+
  annotate(geom = 'rect', xmin = -Inf,xmax = Inf,
           ymin =3,ymax=4,
           fill = "#e66c2a", alpha = 0.2, color = '#ffd2ba')+
  annotate(geom = "text", x=4.4, y=3.2,
           label=paste("Emitter outflow > 3.0 L/h", sep=""),
           color = "#e66c2a", fontface="bold", size=4)+
  geom_hline(yintercept = 3,linetype = "dashed",
             color = "#e66c2a",size=0.8)+
  geom_point(shape = 21, size = 3,
             position = "jitter")+
  geom_smooth(aes(linetype = treat, fill = treat),
              fullrange = T,
              alpha = 0.2, level = 0.95)+
  scale_fill_manual(values = c('#afc978','#a9dcf2','#edc0c0','#ddaaf7'))+
  scale_color_manual(values = c('#394d2d','#2e4587','#f8533d','#69008c'))+
  scale_x_continuous(breaks = seq(-0,16,2),
                     expand = c(0,0.01),limits = c(1,17))+
  scale_y_continuous(breaks = seq(1,4,0.5),
                     expand = c(0,0.01), limits = c(1,4))+
  labs(x = "Operating pressure (m)",
       y = "Emitter outflow (L/h)",
       title = "The Relationship between Operating Pressure and Emitter Outflow")+
  theme_bw()+
  theme(legend.position = "bottom")


ggsave("pf.pdf", height = 4.5, width = 7)
  
       