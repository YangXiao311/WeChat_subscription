setwd("F:\\Desktop\\Boxplot")
library(ggplot2)

df <- data.frame(Time = rep(c('year1','year2','year3','year4','year5'),
                                each = 64),
                     Crop = rep(c('Crop1','Crop2'), each = 32, times = 20),
                     Site = rep(c('Site1','Site2'), each = 16, times = 40),
                     Yield = c(rnorm(16, mean = 22, sd = 3),
                               rnorm(16, mean = 18, sd = 2),
                               rnorm(16, mean = 24, sd = 4),
                               rnorm(16, mean = 22, sd = 2),
                               rnorm(16, mean = 32, sd = 4),
                               rnorm(16, mean = 40, sd = 5),
                               rnorm(16, mean = 34, sd = 4),
                               rnorm(16, mean = 50, sd = 6),
                               rnorm(16, mean = 45, sd = 8),
                               rnorm(16, mean = 75, sd = 7),
                               rnorm(16, mean = 38, sd = 9),
                               rnorm(16, mean = 70, sd = 8),
                               rnorm(16, mean = 52, sd = 12),
                               rnorm(16, mean = 86, sd = 11),
                               rnorm(16, mean = 55, sd = 13),
                               rnorm(16, mean = 85, sd = 14),
                               rnorm(16, mean = 75, sd = 12),
                               rnorm(16, mean = 96, sd = 18),
                               rnorm(16, mean = 58, sd = 14),
                               rnorm(16, mean = 94, sd = 12)))
head(df); str(df)  

theme_set(theme_bw(base_size = 14)+
            theme(plot.title = element_text(size = 17, hjust = 0.5),
                  axis.text = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 12)))

ggplot(df, aes(x = Time, y = Yield, color = Crop)) +
  stat_boxplot(geom = "errorbar", size = 0.8, width = 0.8) +
  geom_boxplot(width = 0.8, size = 0.8, outlier.shape = NA) +
  geom_jitter(size = 1.5, shape = 21, aes(fill = Crop, color = Crop), alpha = 0.6,
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.4)) +
  scale_fill_manual(values = c("Crop2" = "#fcaba2", "Crop1" = "#caf2f9")) +
  scale_color_manual(values = c("Crop2" = "#e64b35", "Crop1" = "#4cbad4")) +
  facet_grid(Site ~ ., scales = "free_y") +
  theme(legend.position = "top") +
  stat_summary(aes(group = Crop), geom = "point", fun = "mean",
               size = 2, shape = 2, color = "black",
               position = position_dodge(0.8)) +
  ylab("Average crop yield") +
  xlab("Operating years")
ggsave("plot1.pdf", height = 6, width = 6)


ggplot(df, aes(x = Time, y = Yield, color = Crop, fill = Crop)) +
  stat_boxplot(geom = "errorbar", size = 0.8, width = 0.8) +
  geom_boxplot(width = 0.8, size = 0.8, outlier.shape = NA) +
  geom_jitter(size = 1.5, shape = 21, aes(fill = Crop, color = Crop), alpha = 0.6,
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.4)) +
  scale_fill_manual(values = c("Crop2" = "#fcaba2", "Crop1" = "#caf2f9")) +
  scale_color_manual(values = c("Crop2" = "#e64b35", "Crop1" = "#4cbad4")) +
  facet_grid(Site ~ ., scales = "free_y") +
  theme(legend.position = "top") +
  stat_summary(aes(group = Crop), geom = "point", fun = "mean",
               size = 2, shape = 2, color = "black",
               position = position_dodge(0.8)) +
  ylab("Average crop yield") +
  xlab("Operating years")
ggsave("plot2.pdf", height = 6, width = 6)
