# Set working directory ------------------------------------------------------------------
setwd("F:\\Desktop\\04-R语言变异柱状图（Variant bar plot in R）")
rm(list = ls())

# Load packages -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(agricolae)
library(tidyr)


# Read data --------------------------------------------------------------------
df_row <- read.csv("df_row.csv", row.names = 1)
treat <-c("Sludge_1","Sludge_2","Sludge_3","Sludge_4","Sludge_5")
index <-c("PN","PS","HA")

df_row$treat <- rep(treat, each = 3)

# Encapsulate ANOVA calculation as a function ------------------------------------------------------------
run_hsd <- function(df, response_var, treatment_var) {
  formula <- as.formula(paste(response_var, "~", treatment_var))
  EPS_aov <- aov(formula, data = df)
  hsd <- HSD.test(EPS_aov, treatment_var)
  result <- hsd$groups
  result$sd <- hsd$means$std
  result <- data.frame(
    treat = rownames(result),
    value = result[[response_var]],
    groups = result$groups,
    std = result$sd,
    index = response_var
  )
  return(result)
}

# Loop throught the indices to calculate PN, PS, HA ------------------------------------------------------------
df1 <- data.frame()

for (i in index){
  formula <- as.formula(paste(i,"~treat"))
  result <- run_hsd(df_row, i, "treat")
  df1 <- rbind(df1, result)
}

# df1 <- do.call(rbind, lapply(index, function(i) run_hsd(df_row, i, "treat")))

df1 <- arrange(df1, index, treat)

write.csv(df1,"df.csv")
df1 <- read.csv("df1.csv", row.names = 1)
df1$index <- factor(df1$index, levels = c("HA","PS","PN"))

# EPS data calculation --------------------------------------------------------------
df2 <- run_hsd(df_row, "EPS", "treat")

# PN/PS data calculation -----------------------------------------------------------------
df3 <- run_hsd(df_row, "PN.PS", "treat")

# Plot --------------------------------------------------------------------
ggplot()+
  geom_bar(data = df2, aes(x = treat, y = value, fill = index),
           stat = "identity", color = "#d03038", width = 0.8,
           fill = "#ffeae7", alpha = 0.3)+
  geom_bar(data = df1, aes(x = treat, y = value, fill = index),
           stat = "identity", position = "stack", width = 0.4)+
  geom_text(data = df1, aes(x= treat, y = value_bar, label = groups),
            position = "identity",vjust=1.2, color="white", size = 5)+
  geom_errorbar(data = df1, aes(x = treat, ymin = value_bar-std, ymax = value_bar+std),
                width = 0.3, size=0.6)+
  scale_fill_manual(values = c("#7a1b1e", "#d03038", "#ff907f"))+
  geom_line(data = df3, aes(x = treat, y = value*150, group = 1), color = "red", size = 1)+
  annotate("text", x=4.8, y=360, label = "PN/PS ratio", color = "red", size = 4)+
  scale_y_continuous(limits = c(0,400), breaks = seq(0,400,100), expand = c(0,0),
                     sec.axis = sec_axis(~./150, name = "PN/PS ratio"))+
  theme_bw()+
  labs(x = "", y = "Content (mg/g)",
       title = "Content of PN, PS, HA, and PN/PS Ratio")+
  theme(
    plot.title = element_text(size = 13, face = "bold.italic"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )


# Save the plot -----------------------------------------------------------
ggsave("plot.pdf", width = 7, height = 4)

