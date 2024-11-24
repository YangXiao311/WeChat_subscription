rm(list = ls())
setwd("F:\\Desktop\\NC论文")
new <- read.csv("df1.csv")
library(ggplot2)
library(lme4)
library(ggpubr)

theme_set(theme_bw(base_size = 14)+
            theme(plot.title = element_text(size=17,hjust=0.5),
                  axis.text = element_text(size = 7),
                  axis.text.y = element_text(size = 10),
                  axis.text.x = element_text(size = 10)))


# figure_5 ----------------------------------------------------------------
#Figure_4_enEMF_Stacked <- 
  
ggplot(aes(x=LUT, y=Value, fill=Service), data=new)+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("Food" = "#f9e26e"  ,
                             "Climate_reg" = "#bde2ea",
                             "Water" = "#a1ccee",
                             "Soil_Health" = "#d8bb75",
                             "Biodiversity" = "#c3d984"))+
  geom_text(aes(label = paste0(round(Value,0),"\n(", 100*round(Value_Percent,3), "%)")), 
            position = position_stack(vjust = 0.5), size = 2.5)+
  geom_text(aes(label = round(enEMF, 0),
                angle=45, y = enEMF+100), 
            col="#387880", size=3.8, hjust=0)+
  
  xlab("LUT")+
  ylab("Ecosystem service value\n(€ / ha / yr)")+
  geom_errorbar(aes(y=enEMF, ymin=enEMF, ymax=enEMF), size=2.2, color="#387880")+
  geom_text(aes(label = round(enEMF_Farmers, 0), angle=45, 
                y = enEMF_Farmers+100), 
            col="#8a5b41", size=3.8, hjust=0)+
  geom_errorbar(aes(y=enEMF_Farmers, ymin=enEMF_Farmers, ymax=enEMF_Farmers),
                size=2.2, color="#8a5b41")
Figure_4_enEMF_Stacked
  ggsave("Figure_4_enEMF_Stacked.pdf",
         Figure_4_enEMF_Stacked, scale = 1,
         width = 8.5, height = 6, dpi=500, units="in")

  

# Figure_3 ----------------------------------------------------------------
mf_data_lt <- read.csv("df2.csv")
  Fig_egEMF_Farmers <- ggplot(aes(y=egEMF_Farmers, x=LUT, color=Climate), data=mf_data_lt)+
    geom_point()+
    scale_colour_manual(values=c("amb" = "#007089", "fut" = "#60c3d9"))+
    ylim(0.3,0.8)+
    stat_summary(
      geom = "point",
      fun.y = "mean",
      size = 5,
      shape = 17)+
    stat_summary(fun.data = mean_se, geom = "errorbar")+ # add errorbar
    ylab("\negEMF\n(farmers' preferences)")+
    theme(text = element_text(size = 22),
          plot.title = element_text(size=18),
          axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18),
          axis.title.y=element_text(size=17))
  
  
  Fig_egEMF_Locals <- ggplot(aes(y=egEMF_Locals, x=LUT, color=Climate), data=mf_data_lt)+
    geom_point()+
    scale_colour_manual(values=c("amb" = "#007089", "fut" = "#60c3d9"))+
    ylim(0.3,0.8)+
    stat_summary(
      geom = "point",
      fun.y = "mean",
      size = 5,
      shape = 17)+
    stat_summary(fun.data = mean_se, geom = "errorbar")+ # add errorbar
    ylab("\negEMF\n(locals' preferences)")+
    theme(text = element_text(size = 22),
          plot.title = element_text(size=18),
          axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18),
          axis.title.y=element_text(size=17))
  
  
  
  
  
  Fig_egEMF_Env <- ggplot(aes(y=egEMF_Env, x=LUT, color=Climate), data=mf_data_lt)+
    geom_point()+
    scale_colour_manual(values=c("amb" = "#007089", "fut" = "#60c3d9"))+
    ylim(0.3,0.8)+
    stat_summary(
      geom = "point",
      fun.y = "mean",
      size = 5,
      shape = 17)+
    stat_summary(fun.data = mean_se, geom = "errorbar")+ # add errorbar
    ylab("egEMF\n(environmental conservation\nagencies' preferences)")+
    theme(text = element_text(size = 22),
          plot.title = element_text(size=18),
          axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18),
          axis.title.y=element_text(size=17))
  
  
  
  
  Fig_egEMF_Tourism <- ggplot(aes(y=egEMF_Tourism, x=LUT, color=Climate), data=mf_data_lt)+
    geom_point()+
    scale_colour_manual(values=c("amb" = "#007089", "fut" = "#60c3d9"))+
    ylim(0.3,0.8)+
    stat_summary(
      geom = "point",
      fun.y = "mean",
      size = 5,
      shape = 17)+
    stat_summary(fun.data = mean_se, geom = "errorbar")+ # add errorbar
    ylab("egEMF\n(tourism sector's\npreferences)")+
    theme(text = element_text(size = 22),
          plot.title = element_text(size=18),
          axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18),
          axis.title.y=element_text(size=17))
  
  Figure_3_egEMF <- ggarrange(Fig_egEMF_Farmers, Fig_egEMF_Locals, Fig_egEMF_Env, Fig_egEMF_Tourism, ncol = 2, nrow = 2, labels = "auto", font.label = list(size = 24), common.legend = TRUE, legend = "bottom")
  Figure_3_egEMF
  
  ggsave("Figure_3_egEMF.pdf", Figure_3_egEMF, scale = 1, width = 10, height = 7, dpi=500, units="in")
  