###############
## MAP NRSA DATA
library(dplyr)
library(ggplot2)
library(sf)
library(maps)
library(mapdata)
library("rnaturalearth")
library("rnaturalearthdata")

# LOAD data stored in Rpackage - a .rda file
load(file="data/nrsa1.rda")


## SPATIAL FILES FOR MAPPING
# GRAB STATE POLYGONS
state<-map_data("state")

## READ SHAPEFILE - copied and pasted ecoregion 9 shapefile on laptop
eco <-read_sf("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/GIS/ECOREG_9/ecoreg9_2015.shp")

#eco<- st_transform(eco, 4269)
eco<-eco %>%
  mutate(AG_ECO5 = case_when (
    (WSA9 %in% c('WMT','XER'))~'West',
    (WSA9 %in% c('NPL','SPL'))~'Great Plains',
    (WSA9 %in% c('UMW','TPL'))~'Midwest',
    (WSA9 %in% c('NAP','SAP'))~'Appalachians',
    (WSA9 %in% c('CPL'))~'Coastal Plains')) %>%
  group_by(AG_ECO5) %>%
  summarise()

# Order ecoregions for gray color assignment
eco$AG_ECO5<-ordered(eco$AG_ECO5, levels=c("West", "Great Plains","Midwest",
                                           "Appalachians","Coastal Plains"))

#############
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# ALL NRSA 2013-14 sites
#NRSA sites
sites <-st_as_sf(nrsa1, coords = c("LON_DD83","LAT_DD83"),
                 crs=4269)

# State outlines
# GRAB STATE POLYGONS
#state<-map_data("state")
states<-st_as_sf(map("state",plot=FALSE, fill=TRUE, color="gray"))

## MAP NLA SITES
map_nrsa13_sites <-ggplot(data=states, color="gray90")+
  geom_sf(data=eco, aes(fill=factor(eco$AG_ECO5))) +
  #geom_sf(data=eco_reduced, fill=NA, size = 0.703, color="black")+
  scale_fill_manual(values=c("#bdbdbd","#969696","#d9d9d9","#737373","#f0f0f0"))+
  #geom_sf(data=eco, aes(fill=factor(eco$ECO_9))) +
  #scale_fill_manual(values=c("#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5",
  #                           "#c7eae5","#80cdc1","#35978f","#01665e"),
  #                 guide = guide_legend())+
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites, size=1)+ # aes(colour=sites$Lake_Origin_use,shape=sites$YEAR),
  #scale_color_manual(values = c("#b2182b","#1a1a1a"),
  #                   guide = guide_legend(override.aes = list(colour= c("#b2182b","#1a1a1a"))))+
  #  shape = c(17, 16,18))))+
  #scale_shape_manual(values = c(17, 16, 18),
  #                   breaks = "2007","2012","2017",
  #                   guide = FALSE)+
  #coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text=element_text(family = "RMN"),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN"),
        legend.title= element_blank(), #element_text(family = "RMN"), # element_blank()
        #panel.border=element_blank(),#
        # panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "bottom",
        legend.box = "vertical")+
  labs(color = "")+
  ggtitle("")
map_nrsa13_sites

###########
## FULL CONUS DATASET FOR
## CONUS n =2069
tiff(filename="inst/output/nrsa1314_conus.tiff", #C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/scripts/Routput/
     width=7.5, height=5, units="in", res=400)
map_nrsa13_sites
dev.off()
