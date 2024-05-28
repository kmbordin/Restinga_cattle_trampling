# map of sampling units ----
library(here)
library(tidyverse)
library(geobr)
library(maptools)
library(grid)
library(maps)
library(ggsn)
library(ggspatial)

latlong <- read.table(here("data", "coordenadas.txt"), h=T)
disturbance <- data.frame(site = c("RB1","RB2","RB3","RH5","RC6","RA7","RE9","RE10","RH4"), 
                          disturbance = c("medio","medio","alto","ausente","medio","alto","alto","alto","ausente"))

data <- merge(latlong,disturbance, by.x="site", by.y="site")
env.BF <- data %>% 
  mutate(disturbance = replace(disturbance, disturbance == "alto", "High")) %>% 
  mutate(disturbance = replace(disturbance, disturbance == "medio", "Intermediate")) %>% 
  mutate(disturbance = replace(disturbance, disturbance == "ausente", "Low")) 

datasets <- list_geobr()
datasets
meso <- read_municipality(year = 2010)
meso <- filter(meso, name_muni=="Capão Do Leão")
#save(meso, file = "R/meso.RData")

my_theme <- theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(#legend.position = "none",
    text = element_text(size=9),
    plot.margin=unit(c(0.1, 0.1, 0.1 , 0.1), "cm"))

base.plot <- ggplot() +
  geom_sf(data=meso, fill="white")+xlim(-52.435, -52.395) + ylim(-31.8, -31.82) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "gray80", size = .5, fill = NA))  +
  ylab("Latitude") + xlab("Longitude");base.plot

site <- base.plot+
  my_theme+
  geom_point(data = env.BF, aes(x = longitude, y = latitude, color=disturbance, size=2))+
  #geom_text(data = env.BF, aes(x=longitude,y=latitude,label = site, size=.5))
  scale_color_manual(values = c("#D55E00","#0072B2","#CC79A7"))+
  guides(colour = guide_legend(title = "Disturbance"))+
  theme(legend.justification = 'center', 
        legend.position = 'bottom', 
        legend.text = element_text(size=12))+
  scalebar(x.min = -53.5, x.max = -48,
           y.min = -30, y.max = -25,
           dist = 40, dist_unit = "km",
           st.bottom = FALSE, st.color = "gray20",box.fill = c("gray30", "white"),box.color = "gray30",
           transform = TRUE, model = "WGS84",st.size=2.5)+
  guides(size = "none");site

site <- site+ 
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey90", "white"),
      line_col = "grey60"))+
  annotation_scale()

# tiff("plot_geral.tif", height=6, width=5,units = "in",res=300)
# plot_geral
# dev.off()


mapa <- site+  annotation_custom(ggplotGrob(plot.geral), xmin =-43, xmax = -46,ymin = -30, ymax = -26)+
  #annotation_custom(ggplotGrob(site), xmin = -78, xmax = -58,ymin = -36, ymax = -17)+
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey90", "white"),
      line_col = "grey60"
    )
  )
mapa

#tiff("para_cpiar.tif", height =55, width =63,units = "in",res=300)--site
#tiff("siteboth.tif", height =7, width =18,units = "in",res=300)-- mapa
# dev.off()