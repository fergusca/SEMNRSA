#########################
## DATA EXPLORE _ ADDITIONAL ANALYSES FOR MS _ DRIVERS OF NRSA BUGS WEST
##
## 1/30/2023
#########################

remove(list=ls())

library(dplyr)
library(ggplot2)
library(sp)
library(gstat)
library(scales)
library(tidyr)

library(maps)
library(maptools)

library(sf)
library(readr)

##########
## READ PROCESSED DATA
# COMPILED NRSA SURVEYS WITH SUBSET OF VARIABLES
#  INCLUDES ALL RESAMPLED SITES AND VISITS 1 & 2

# PROCESSED - all 0809 and only new sites from later surveys
#  VISITS 1 and 2 n = 4578
dat_org <- read.csv("data_processed/Compiled/nrsa081318_nonresampled_VISIT_12.csv")

# PROCESSED DATA VISIT_NO=1 ONLY n = 4389
dat_proc<-dat_org%>%
  filter(VISIT_NO==1)

###############
## PROCESS DATA DROPPING MISSING PROTOCOL
table(dat_proc$AG_ECO9)
dat_proc$PROTOCOL<-as.factor(dat_proc$PROTOCOL)
summary(dat_proc$PROTOCOL)

# n = 4371
dat_proc<- dat_proc%>%
  drop_na(PROTOCOL)%>%
  filter(PROTOCOL=="BOATABLE"|PROTOCOL=="WADEABLE")

# DROP NOPHAB class from REALM
dat_proc$PROTOCOL<-droplevels(dat_proc$PROTOCOL)
table(dat_proc$PROTOCOL)
#BOATABLE WADEABLE
#1635     2736

table(dat_proc$PROTOCOL,dat_proc$AG_ECO9)

table(dat_proc$AG_ECO3)
#EHIGH PLNLOW  WMTNS
#1141   2271    959

# Transform watershed and catchment area
dat_proc<- dat_proc%>%
  mutate(L_WsAreaSqKm = log10(WsAreaSqKm))%>%
  mutate(L_CatAreaSqKm = log10(CatAreaSqKm))

summary(dat_proc$L_WsAreaSqKm)


#########################
## SUBSET BY AGGREGATED 9- ECOREGION

# WMT
wmt<-dat_proc%>%
  filter(AG_ECO9=="WMT")%>%
  drop_na(LOE_QLow_cl)

summary(wmt$LOE_Qbkf_cl)

# SCALE CUMULATIVE PRECIPITATION in wmt
wmt$PSUMPY_SY_WS_sc<-scale(wmt$PSUMPY_SY_WS)
summary(wmt$PSUMPY_SY_WS_sc)

# WADEABLE n = 323
wmt_w <- wmt %>%
  filter(PROTOCOL=="WADEABLE")

# BOATABLE n = 167
wmt_b <-wmt %>%
  filter(PROTOCOL=="BOATABLE")

##############
## XER
xer<-dat_proc%>%
  filter(AG_ECO9=="XER")%>%
  drop_na(LOE_QLow_cl)

summary(xer$LOE_Qbkf_cl)

# SCALE CUMULATIVE PRECIPITATION in xer
xer$PSUMPY_SY_WS_sc<-scale(xer$PSUMPY_SY_WS)
summary(xer$PSUMPY_SY_WS_sc)

# WADEABLE n = 272
xer_w <- xer %>%
  filter(PROTOCOL=="WADEABLE")

# BOATABLE n = 143
xer_b <-xer %>%
  filter(PROTOCOL=="BOATABLE")


##########################
## LOOK AT MMI AND RIPARIAN WOODY COVER METRICS
# SUBSET DATA
wmt_sub<-wmt_w %>%
  filter(PCTURB_WS<10)%>%
  filter(PCTFOR_WS>69)

wmt_red<-wmt_sub%>%
  select(SITE_ID, VISIT_NO,MMI_BENT,LRBS_use,XCMGW,XGB,XGW,XMW,XC)


# MELT Woody columns into single column and list whether XGW, XMW, XC
wmt_red<-cbind(wmt_red[1:5],stack(wmt_red[6:9]))
wmt_red<- wmt_red%>%
  rename("WOODY"="ind")#%>%
  #rename("Riparian index"= values)
head(wmt_red)

# RIPARIAN AND MMI
rip_mmi<-ggplot(wmt_red, aes(x=values, y=MMI_BENT, color=WOODY)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, level=0.95)+
  theme_bw(base_size=12)+
  theme(legend.position = "none")+
  ylab("MMI")+
  xlab("Riparian index")+
  ggtitle("Riparian layer effects on MMI")

rip_mmi

## RIPARIAN AND RBS
rip_rbs<-ggplot(wmt_red, aes(x=values, y=LRBS_use, color=WOODY)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, level=0.95)+
  theme_bw(base_size=12)+
  theme(legend.position = "bottom")+
  ylab("log10 RBS")+
  xlab("Riparian index")+
  ggtitle("Relative bed stability")

rip_rbs

# Marginal density distribution of woody riparian veg by type
xdensity<-ggplot(wmt_red,aes(values, fill=WOODY))+
  geom_density(alpha=.5)+
  theme_bw(base_size=12)+
  theme(legend.position = "right")+
  ggtitle("Marginal density distribution of woody riparian veg type")
xdensity

summary(lm(wmt_sub$MMI_BENT~wmt_sub$XGB)) # B = 14.9 p = 0.04
summary(lm(wmt_sub$MMI_BENT~wmt_sub$XGW)) #B=54.55 p<0.001
summary(lm(wmt_sub$MMI_BENT~wmt_sub$XMW)) #B = 37.7 p<0.001
summary(lm(wmt_sub$MMI_BENT~wmt_sub$XC)) #B=35.3 p<0.001

##################
## PRINT PLOTS RIPARIAN XGW, XMW, XC vs MMI
# GET LEGEND
legend<-get_legend(rip_rbs)

# REMOVE LEGEND
rip_rbs<-rip_rbs + theme(legend.position="none")

# SCATTERPLOTS RIPARIAN COMPONENTS AND MMI AND RBS
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Scatterplot_XCMGW_parts_WMTw.tiff",
     width=7.5, height=5, units="in", res=400)
grid.arrange(arrangeGrob(rip_mmi,
                         rip_rbs,
                         ncol=2,widths=c(3.5,3.5)),
             legend,nrow=2, heights=c(4, .5))
dev.off()

##########################
# Spatial Introduction
# https://rpubs.com/nabilabd/118172

# COULD TRY TO INTERPOLATE VALUES ACROSS ECOREGION TO HELP DETERMINE WHAT MAY BE CORRELATED

## Look at O/E SCORES in XER wadeable streams
xer_w %>% as.data.frame%>%
  ggplot(aes(LON_DD83,LAT_DD83)) + geom_point(aes(size=OE_SCORE), color="blue", alpha=3/4)+
  ggtitle("OE Scores XER wadeable") + coord_equal() + theme_bw()

# Agriculture
xer_w %>% as.data.frame%>%
  ggplot(aes(LON_DD83,LAT_DD83)) + geom_point(aes(size=asin_PCTAGR_WS), color="blue", alpha=3/4)+
  ggtitle("% agriculture XER wadeable") + coord_equal() + theme_bw()

# Summer low flow
xer_w %>% as.data.frame%>%
  ggplot(aes(LON_DD83,LAT_DD83)) + geom_point(aes(size=LQLow_kmcl), color="blue", alpha=3/4)+
  ggtitle("Summer flow XER wadeable") + coord_equal() + theme_bw()


class(xer_w) # data.frame


xer_sp<-xer_w%>%
  filter(!is.na(OE_SCORE))
# DROP obs with missing OE_SCORE

# MAKE INTO A SPATIAL DATAFRAME BY SPECIFYING WHICH COLUMNS CONTAIN THE COORDINATES
coordinates(xer_sp) <- ~ LON_DD83 + LAT_DD83
class(xer_sp)

# Fitting a variogram
oe.vg <- variogram(OE_SCORE~1, xer_sp)
plot(oe.vg)
oe.fit<- fit.variogram(oe.vg,model=vgm(1,"Sph",1,1))
plot(oe.vg, oe.fit)


plot1<- xer_sp %>% as.data.frame %>%
  ggplot(aes(LON_DD83, LAT_DD83)) + geom_point(size=1) + coord_equal() +
  ggtitle ("Points with measurments")

plot2<- xer_sp.grid %>% as.data.frame %>%
  ggplot(aes(LON_DD83, LAT_DD83)) + geom_point(size=1) + coord_equal() +
  ggtitle ("Points at which to estimate")

library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

##############################
## READ SHAPEFILE -
#Aggregated Ecoregion 9
eco <-read_sf("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/GIS/ECOREG_9/ecoreg9_2015.shp")
names(eco)
table(eco$WSA9_NAME)


# GRAB STATE POLYGONS
states <-st_as_sf((map("state",plot=FALSE,fill=TRUE)))
head(states)

################
## MAKE MAP
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
###############
eco<- st_transform(eco, 4269)
# Order ecoregions for gray color assignment
eco$WSA9<- ordered(eco$WSA9, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

eco_xer <-eco %>%
  filter(WSA9=="XER") #filter(ECO_5 == "West"|ECO_5 == "Midwest")

eco_wmt <-eco %>%
  filter(WSA9=="WMT")

###########################
# NRSA SITES: WMT + XER WADEABLE
nrsa_wmtxerw <- dat_proc %>%
  filter(AG_ECO9=="WMT"|AG_ECO9=="XER",PROTOCOL=="WADEABLE")

# NRSA SITES: XER WADEABLE
nrsa_xerw <- dat_proc %>%
  filter(AG_ECO9=="XER",PROTOCOL=="WADEABLE")

#NRSA SITES:WMT WADEABLE (subset forested watersheds >70%, <10% urban)
wmt_sub<-wmt_w %>%
  filter(PCTURB_WS<10)%>%
  filter(PCTFOR_WS>69)
#############
# ALL NRSA sites n = 4389
sites <-st_as_sf(dat_proc, coords = c("LON_DD83","LAT_DD83"),
                 crs=4269)

############
# WMT + XER WADEABLE n = 626
sites_wmtxer <-st_as_sf(nrsa_wmtxerw, coords = c("LON_DD83","LAT_DD83"),
                        crs=4269)
# XER WADEABLE n = 341
sites_xer <-st_as_sf(nrsa_xerw, coords = c("LON_DD83","LAT_DD83"),
                     crs=4269)

# WMT WADEABLE SUBSET n = 155
sites_wmtred <- st_as_sf(wmt_sub, coords = c("LON_DD83","LAT_DD83"),
                         crs=4269)
####################
## MAP ALL NRSA SITES THREE SURVEYS PROCESSED TO NOT INCLUDE RESAMPLED
#   n = 4389
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman


map_all <-ggplot()+ #data=world, color="gray90"
  geom_sf(data=eco_xer, aes(fill=factor(WSA9))) +
  #scale_fill_manual(values=c("#bf812d"))+ # #"#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"
  #scale_fill_grey(start=0.3,end=0.8, guide=FALSE)+
  #geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites_xer, aes(colour=PROTOCOL),size = .5)+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  scale_color_manual(values = "black") +#scale_color_manual(values= c("#e41a1c","#377eb8"),na.value = "#404040")+ #"gray"
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
        legend.title= element_blank(), #element_text(family = "RMN"), #
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


# MAP OF WMT wadeable subset of streams
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://www.r-bloggers.com/2017/08/simple-practice-basic-maps-with-the-tidyverse/
map_wmtsub <-ggplot()+ #data=world, color="gray90"
  geom_sf(data=eco_wmt,aes(fill=factor(WSA9))) +
  scale_fill_manual(values=c("#f0f0f0"))+ # #"#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites_wmtred , aes(colour=PROTOCOL),size = .8)+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  scale_color_manual(values = "red") +#scale_color_manual(values= c("#e41a1c","#377eb8"),na.value = "#404040")+ #"gray"
  #geom_sf_label(data=eco, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -102), ylim = c(24, 51), expand = FALSE)+
  #coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        #axis.text=element_text(family = "RMN"),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(), # remove lat long values
        axis.text.y = element_blank(),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN"),
        legend.title= element_blank(), #element_text(family = "RMN"), #
        panel.border=element_blank(),#
        panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.box = "vertical",
        legend.position = "none")+
  labs(color = "Stream type", fill = "")+
  ggtitle("Western Mountain wadeable streams in forested watersheds\n n=155") #"Hydrologic alteration potential"

map_wmtsub

######################
## PRINT MAP
## WMT w subset forested watersheds (>70%) with low urban (<10%) n=155
## TO share with Beth Sosik looking at relationship among riparian features and MMI
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Maps/WMTw_subset_155.tiff",
     width=7.5, height=5, units="in", res=200)
map_wmtsub
dev.off()
