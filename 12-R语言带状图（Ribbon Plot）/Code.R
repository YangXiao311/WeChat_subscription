setwd("F:\\Desktop\\Test")
library(ggplot2)
library(readxl)

df_pre <- read_xlsx("df.xlsx", sheet = "Sheet1")
df_tem <- read_xlsx("df.xlsx", sheet = "Sheet2")
df_dif <- read_xlsx("df.xlsx", sheet = "Sheet3")

theme_set(theme_bw(base_size = 14)+
            theme(plot.title = element_text(size = 17, hjust = 0.5),
                  axis.text = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 12),
                  panel.grid = element_blank()))

ggplot()+
  geom_col(data = df_pre, aes(x = days, y = value*1.5, fill = factor(year)),
           position = "dodge", width = 3)+
  geom_ribbon(data = df_dif, aes(x = days, ymax = MAX_temp, ymin = MIN_temp,
                                 fill = factor(year)),alpha = 0.20)+
  scale_fill_manual(values = c("2023" = "#e62426", "2024" = "#5593c4"))+
  geom_line(data = df_tem, aes(x = days, y = value, color = factor(year)))+
  scale_color_manual(values = c("2023" = "#e62426", "2024" = "#5593c4"))+
  scale_y_continuous(limits = c(-5, 46), breaks = seq(-5,45,10),
                     sec.axis = sec_axis(~./1.5, name = "Precipitation (mm)"))+
  labs(x = "Day post-sowing in 2023 and 2024 (Days)",
       y = "Temperature (C\u00B0)")+
  theme(legend.position = "top")

ggsave("fig.pdf", width = 6, height = 4)
