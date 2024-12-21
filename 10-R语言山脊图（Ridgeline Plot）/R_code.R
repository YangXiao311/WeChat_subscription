library(ggplot2)
library(readxl)
#install.packages("ggdist")
#install.packages("MetBrewer")
library(MetBrewer)
library(ggdist)
library(tidyverse)
library(ggtext)

setwd("F:/Desktop/Ridgeline")


# Evap --------------------------------------------------------------------
data <- read_xlsx("evaporation_sum_2020_2024.xlsx")
Sys.setlocale("LC_TIME", "English")
data <- data %>%
  mutate(date = as.Date(date), 
         month = months(date, abbreviate = TRUE))
data$month <- factor(data$month, levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                            "Jul","Aug","Sep","Oct","Nov","Dec"))
head(data)
median_evap <- median(data$value)
#mean_evap <- mean(data$value)

ggplot(data, aes(month, value))+
  stat_halfeye(fill_type = "segments", alpha = 0.4)+
  stat_interval(size = 2.5)+
  stat_summary(geom = "point", fun = median)+
  geom_hline(yintercept = median_evap, col = "grey30", lty = "dashed")+
  annotate("text", x = 12.5, y = median_evap + 0.1,
           label = "Median Evaporation", size = 3, hjust = 0) +
  stat_summary(aes(y = value), geom = "text",
    fun.data = function(x) {
      data.frame(y = -0.55, label = sprintf("(%s)", scales::number(mean(x, na.rm = TRUE),accuracy = 0.1)))},
    size = 2.5) +
  scale_x_discrete(labels = toupper)+
  scale_y_continuous(breaks = seq(0.0, 6.0, 1.0))+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"))+
  coord_flip(ylim = c(0, 6), clip = "off") +
  guides(col = "none") +
  labs(y = "Evaporation from bare soil (mm)",
       x = NULL)+
theme(plot.background = element_rect(color = NA, fill = "grey97"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(
        margin = margin(t = 4, b = 16), size = 10),
      plot.caption = element_textbox_simple(
        margin = margin(t = 12), size = 7),
      plot.caption.position = "plot",
      axis.text.y = element_text(hjust = 0, margin = margin(r = 17)),
      plot.margin = margin(1, 2, 2, 2))  
ggsave("evap.pdf", width = 4.8, height = 3)
