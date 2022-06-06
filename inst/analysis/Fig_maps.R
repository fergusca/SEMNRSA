##########################
## STUDY EXTENT MAPS NRSA SEM PROJECT
##
## 5/4/2022
###########################

remove(list=ls())

library(ggplot2)
library(ggmap)
library(dplyr)
library(gridExtra)
library(cowplot)
library(tidyr)

library(maps)
library(mapdata)
library(scales)
library(maptools)

library(sf)
library(readr)
library(rgeos)

library("rnaturalearth")
library("rnaturalearthdata")

###########
## Plot specifications
###########
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

################
## LOAD DATA
# PROCESSED - all 0809 and only new sites from later surveys
#  VISITS 1 and 2 n = 4578
dat_org <- read.csv("data_processed/Compiled/nrsa081318_nonresampled_VISIT_12.csv")

# PROCESSED DATA VISIT_NO=1 ONLY n = 4389
dat_proc<-dat_org%>%
  filter(VISIT_NO==1) %>%
  filter(PROTOCOL=="BOATABLE"|PROTOCOL=="WADEABLE")

## READ SHAPEFILE -
eco <-read_sf("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/GIS/ECOREG_9/ecoreg9_2015.shp")
names(eco)
table(eco$WSA9_NAME)

#eco5<-eco %>%
#  mutate(ECO_5 = case_when (
#    (WSA9 %in% c('WMT','XER'))~'West',
#    (WSA9 %in% c('NPL','SPL'))~'Great Plains',
#    (WSA9 %in% c('UMW','TPL'))~'Midwest',
#    (WSA9 %in% c('NAP','SAP'))~'Appalachians',
#    (WSA9 %in% c('CPL'))~'Coastal Plains')) %>%
#  group_by(ECO_5) %>%
#  summarise()

################
## MAKE MAP
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
###############
eco<- st_transform(eco, 4269)
# Order ecoregions for gray color assignment
eco$WSA9<- ordered(eco$WSA9, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

#eco$ECO_5<-ordered(eco$ECO_5, levels=c("West", "Great Plains","Midwest",
#                                       "Appalachians","Coastal Plains"))

#dat$ECOREG_rev<-ordered(dat$ECOREG_rev, levels=c("West", "Great Plains","Midwest",
#                                                 "Appalachians","Coastal Plains"))

eco_reduced <-eco %>%
  filter(WSA9 == "WMT") #filter(ECO_5 == "West"|ECO_5 == "Midwest")

#eco$ECO_5<-ordered(eco$ECO_5, levels=c("Great Plains","Appalachians","West","Midwest",
#                                       "Coastal Plains"))

##################
## MAP
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
## GRAB WORLD DATA
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# GRAB STATE POLYGONS
state<-map_data("state")

###########################
# NRSA SITES: WMT WADEABLE
nrsa_wmtw <- dat_proc %>%
  filter(AG_ECO9=="WMT",PROTOCOL=="WADEABLE")

#############
# ALL NRSA sites n = 4389
sites <-st_as_sf(dat_proc, coords = c("LON_DD83","LAT_DD83"),
                 crs=4269)

# WMT WADEABLE n = 341
sites_wmt <-st_as_sf(nrsa_wmtw, coords = c("LON_DD83","LAT_DD83"),
                     crs=4269)

###############
# State outlines
#states<-st_as_sf(map("state",plot=FALSE, fill=TRUE, color="gray"))


#####################
## MAP ALL NRSA SITES THREE SURVEYS PROCESSED TO NOT INCLUDE RESAMPLED
#   n = 4389
#####################

map_all <-ggplot(data=world, color="gray90")+
  geom_sf(data=eco, aes(fill=factor(eco$WSA9))) +
  scale_fill_manual(values=c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                             "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  #scale_fill_grey(start=0.3,end=0.8, guide=FALSE)+
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites, aes(colour=sites$PROTOCOL),size = .5)+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  scale_color_manual(values= c("#e41a1c","#377eb8"),na.value = "#404040")+ #"gray"
  #geom_sf_label(data=eco, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        #axis.text=element_text(family = "RMN"),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(), # remove lat long values
        axis.text.y = element_blank(),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN"),
        legend.title=element_text(family = "RMN"), # element_blank()
        panel.border=element_blank(),#
        panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.box = "vertical",
        legend.position = "bottom")+
  labs(color = "Stream type", fill = "")+
  ggtitle("") #"Hydrologic alteration potential"

map_all


#####################
## MAP WMT WADEABLE n = 341
#####################

map_wmt <-ggplot(data=world, color="gray90")+
  geom_sf(data=eco,fill="gray78", color="grey94")+ # fill=NA  aes(fill=factor(eco$WSA9))) +
  geom_sf(data=eco_reduced, aes(fill=factor(eco_reduced$WSA9))) +
  scale_fill_manual(values=c("#8c510a"))+
  #scale_fill_grey(start=0.3,end=0.8, guide=FALSE)+
  #scale_fill_manual(values=c("#bdbdbd","#ffffff","#d9d9d9","#ffffff","#ffffff"), guide=FALSE)+
  #geom_sf(data=states, fill=NA, color="gray83")+
  geom_sf(data=sites_wmt, color="black",size =0.7)+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  #scale_color_manual(values= c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value = "#404040")+ #"gray"
  #geom_sf_label(data=eco_reduced, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        #axis.text=element_text(family = "RMN"),
        #axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(), # remove lat long values
        axis.text.y = element_blank(),
        legend.text = element_text(family = "RMN", size=12),
        legend.title=element_text(family = "RMN"), # element_blank()
        panel.border=element_blank(),#
        panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "none")+
  #labs(color = "HydrAP rank")+
  ggtitle("") #"Hydrologic alteration potential"

map_wmt

###########
## PRINT MAPS

# GET LEGEND
legend<-get_legend(map_all)

# REMOVE LEGEND
map_all_nl<-map_all + theme(legend.position="none")

# REVISED MINIMALLY DISTURBED AND FULL CONUS MAPS
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Maps/NRSA_conus_4371_w_legend.tiff",
     width=7.5, height=7, units="in", res=400)
grid.arrange(arrangeGrob(map_all_nl,
                         legend, nrow = 2),
             heights=c(4.5,1))
#grid.arrange(west_pct_eff_stacked_bar, midwest_pct_eff_stacked_bar, legend, ncol=2, nrow=2, widths=c(3.0, 2.8))
dev.off()


## CONUS non-resampled VISIT_NO 1
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Maps/NRSA_conus_4371.tiff",
     width=7.5, height=5, units="in", res=200)
map_all
dev.off()


# WMT
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Maps/WMT_wade.tiff",
     width=7.5, height=5, units="in", res=200)
map_wmt
dev.off()
