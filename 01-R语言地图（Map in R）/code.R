setwd("F:\\Desktop\\Github\\01-R语言地图（Map in R）")
library(readxl)
library(ggplot2)
library(dplyr)
library(stars)
library(ggspatial)

# point_data ---------------------------------------------------------------
point_site <- read_xlsx("city_site.xlsx")
point_data <- data.frame(id = 1:201,
                        value = runif(201, 1, 10),
                        group1 = rep(c('A-index','B-index','C-index'),
                                    each = 67),
                        group2 = c(rep('group-A', each = 100),rep('group-B', times = 101)))
point_data <- merge(point_site, point_data)
df_st_as_sf <- st_as_sf(point_data, coords = c("lon", "lat"),crs = 4326)

# province_data ----------------------------------------------------------------
pro_data <- read_xlsx("pro_data.xlsx")
china_map <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[c("adcode", "name", "geometry")]
china_map <- merge(china_map, pro_data, by.x = "name", by.y = "province", all.x = TRUE)
china_map <- mutate_all(china_map, ~replace(., is.na(.), 0))
nat.earth <- raster::brick("HYP_50M_SR_W.tif")

# data_range --------------------------------------------------------------
myfun <- function(value) {
  dis = ""
  if(value <= 10)
    dis = "0-10"
  if(value > 10 & value <= 20)
    dis = "11-20"
  if(value > 20 & value <= 30)
    dis = "21-30"
  if(value > 30 & value <= 40)
    dis = "31-40"
  if(value > 40 & value <= 50)
    dis = "41-50"
  if(value > 50)
    dis = ">50"
  dis
}
china_map$dis <- sapply(china_map$index, function(x) myfun(x))
china_map$dis <- factor(china_map$dis,
                           levels = c("0-10","11-20","21-30","31-40",
                                      "41-50",">50"))

# Plot --------------------------------------------------------------------
ggplot(china_map)+
  #layer_spatial(nat.earth)+
  theme_bw()+
  geom_sf(color='black',size=0.8, aes(fill = dis))+
  geom_sf(df_st_as_sf, mapping = aes(color = group1, shape = group2, size = value), alpha = 0.4)+
  scale_fill_manual(values = c("#f7fbfe","#9fcbe2","#6badd7","#4292c7","#07519c","#08306c"))+
  scale_color_manual(values = c("#753f93","#fddb2f","#fcd6a5"))+#A\B颜色设置
  annotation_north_arrow(location = "tl", which_north = F,
                         pad_x = unit(0.05, "in"),
                         pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        panel.background = element_rect("#f5f6f1"))+#地图底层颜色设置
  coord_sf(ylim = c(-3687082,1654989),
           xlim = c(-3000000,2700000),
           crs = "+proj=laea +lat_0=40 +lon_0=104")+
  annotation_scale(location="bl",width_hint=0.3)+
  labs(x='', y='',color=NULL)
ggsave("Map_plot.pdf", width = 7, height = 4.5)


