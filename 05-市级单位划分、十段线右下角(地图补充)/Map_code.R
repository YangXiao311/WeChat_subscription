# Path --------------------------------------------------------------------
setwd("F:\\Desktop\\TEST1")
rm(list = ls())


# Package -----------------------------------------------------------------
library(readxl)
library(stars) 
library(ggplot2)
library(ggspatial)
library(cowplot)
# Data --------------------------------------------------------------------
### --- Mainland ---
china_map <- sf::st_read("china_city.geojson")[c("adcode", "name", "geometry")]

province_anno <- read_xlsx("province_anno.xlsx")

province_data <- data.frame(city = province_anno$name,
                            value = round(runif(nrow(province_anno),0,10),0))

n1 <-  round(runif(3,0,371),0)
province_data$value[c(n1)] <- runif(3,50,55)
n2 <- round(runif(15,0,371),0)
province_data$value[c(n2)] <- runif(15,35,50)
n3 <- round(runif(20,0,371),0)
province_data$value[c(n3)] <- runif(20,11,35)

china_map <- merge(china_map, province_anno, by.x = "name", by.y = "name", all.x = TRUE)
china_map <- merge(china_map, province_data, by.x = "name", by.y = "city", all.x = TRUE)
china_map <- dplyr::mutate_all(china_map, ~replace(., is.na(.), 0))
Count <- read_xlsx("city_site.xlsx")
Count$number <- round(runif(nrow(Count),0,51),0)

df_st_as_sf <- st_as_sf(Count,coords = c("lon", "lat"),crs = 4326)

myfun <- function(value) {
  stars = ""
  if(value <= 1)
    stars = "0-1"
  if(value > 1 & value <= 3)
    stars = "1-3"
  if(value > 3 & value <= 6)
    stars = "3-6"
  if(value > 6 & value <= 10)
    stars = "6-10"
  if(value > 10 & value <= 20)
    stars = "10-20"
  if(value > 20 & value <= 50)
    stars = "20-50"
  if(value > 50)
    stars = ">50"
  stars
}
china_map$signif <- sapply(china_map$value, function(x) myfun(x))
china_map$signif <- factor(china_map$signif,
                           levels = c("0-1","1-3","3-6","6-10","10-20","20-50",">50"))

### --- Nine-dash line ---
province_data1 <- read_xlsx("prov_data.xlsx")
china_map2 <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[c("adcode", "name", "geometry")]
china_map2 <- merge(china_map2, province_data1, by.x = "name", by.y = "province", all.x = TRUE)


# Plot --------------------------------------------------------------------
### --- Mainland ---
map1 <- ggplot(china_map)+
  theme_bw()+
  geom_sf(color='black',size=0.8, aes(fill = signif))+
  geom_sf(df_st_as_sf,mapping=aes(color=group, size = number), alpha = 0.6)+
  scale_fill_manual(values = c("#fcfaf6","#efe7d1","#e2c4a6","#d59e85","#a85261","#833151","#52123a"))+
  scale_color_manual(values = c("#2f7fc1","#fa530e","#64b605"))+#A\B颜色设置
  annotation_scale(location="bl",width_hint=0.3)+
  annotation_north_arrow(location = "tl", which_north = F,
                         pad_x = unit(0.05, "in"),
                         pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        panel.background = element_rect("white"))+#地图底层颜色设置
  coord_sf(ylim = c(-2787082,1654989),
           crs = "+proj=laea +lat_0=40 +lon_0=104")+
  labs(x='', y='',color=NULL)
map1


### --- Ten-dash line ---
map2 <-ggplot(china_map2)+
  geom_sf(color='black',aes(fill = index),size=0.8)+
  scale_fill_gradient2(low = "grey", mid = "white", high = "#efe7d1")+#右下角小地图颜色设置
  coord_sf(ylim = c(-3928017,-1577944),xlim = c(117131.4,2115095),
           crs = "+proj=laea +lat_0=40 +lon_0=104")+
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.background = element_rect("white"))#小地图底层颜色设置
map2

# Merge -------------------------------------------------------------------

ggdraw() +
  draw_plot(map1) +
  draw_plot(map2, x = 0.675, y = -0.033, width = 0.16, height = 0.48)
ggsave("map.pdf", height = 5, width = 6.5)

