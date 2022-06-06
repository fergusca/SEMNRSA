##################
## BARCHARTS OF TOTAL EFFECTS OF PREDICTORS
## RESPONSE MMI
## WESTERN WADEABLE SITES
## Model 9 revised
##
## 5/31/22
###################

remove(list=ls())

library(dplyr)
library(ggplot2)
#library(grid)
library(gridExtra)
#library(lattice)
library(cowplot)
library(GGally)
library(ggpubr)
library(tidyr)
#install.packages("RColorBrewer")
library(RColorBrewer)

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# SET COLOR PALATE FOR BY DRIVER CATEGORY
# For barchars with individual predictors
# Color brewer qualitative Dar2
#cbdark2 <- c("#7570b3","#666666","#1b9e77","#66a61e","#d95f02","#e6ab02","#e7298a")
#cbdark2_d <- c("#66a61e","#e6ab02","#e7298a")
#cbdark2_i <- c("#7570b3","#666666","#1b9e77","#66a61e","#d95f02")
#cbPalette <- c("#8c510a","#bf812d","#dfc27d","#c7eae5","#80cdc1","#35978f","#01665e")

# Viridis Color palette
# https://waldyrious.net/viridis-palette-generator/
vid <- c("#440154","#443983","#31688e","#21918c","#440154","#35b779","#90d743","#fde725")
vid_d <- c("#443983","#443983","#31688e","#21918c","#35b779","#90d743","#fde725")
vid_i <- c("#440154","#443983","#31688e","#21918c","#443983","#35b779")

# REVERSE ORDER TO HAVE COLORS ACENDING UP STACKED BARCHART
#cbPalette_rev <- c("#F0E442","#56B4E9","#d9d9d9","#bdbdbd","#999999","#E69F00")

#######################
## LOAD DATA

## ALL MODEL OUTPUT OE, MMI, EPT
teff <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Total_effects/WMT_w_m9_ALL_tot.csv")

## WESTERN WADEABLE MODEL 1 MMI OUTPUT
# HAND COMPILED TOTAL EFFECTS (Standardized with Std error) from SEM model output with bootstrap sampling
#teff <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Total_effects/WMT_w_m1_MMI_tot.csv")

names(teff)
table(teff$predictor)

##############
## PROCESS DATA
# RELABEL PREDICTOR VARIABLES
teff$predictor_2<-teff$predictor
teff <- teff %>%
  mutate(predictor_2= recode(predictor_2,
                             asin_PCTAGR_WS="Agr Ws", asin_PCTURB_WS="Urban Ws", PSUMPY_SY_WS_sc="Precipitation",Tmax8110Ws="Max Temp",phdi_mean="PHDI",L_NABD_NrmStorWs_ratio="Dam",
                             L_SLOPE_DPTH="Slope*depth",asin_PCTFOR_WsRp100="Forest Rp",  asin_PCTWET_WsRp100="Wetland Rp",LQbkf_kmcl="Bankfull flow", LQLow_kmcl="Low flow",
                             d.excess_sc="d-excess", Lpt01_XCMGW="Riparian cover", Lpt01_XFC_NAT="Instream cover",LRBS_use="Bed stability",
                             W1_HAG = "Agr index", L_NTL="TN",L_SULF="SO4")) #Lpt01_XFC_NAT="Instream cover",

table(teff$predictor_2)

# ORDER PREDICTOR VARIABLES
teff$predictor_2 <- ordered(teff$predictor_2, levels=c("Agr Ws","Urban Ws","Dam","Precipitation","Max Temp","PHDI",
                                                       "Bankfull flow","Low flow","d-excess","Slope*depth",
                                                       "Agr index",
                                                       "Forest Rp","Wetland Rp","Riparian cover",
                                                       'Instream cover',"Bed stability","TN","SO4")) #"Instream cover","agr",
table(teff$predictor_2)


# CREATE CATEGORIES
teff$category <-teff$predictor
teff <- teff %>%
  mutate(category = recode(category,
                           asin_PCTAGR_WS="Landuse",asin_PCTURB_WS="Landuse",L_NABD_NrmStorWs_ratio="Landuse",
                           Tmax8110Ws="Climate",PSUMPY_SY_WS_sc="Climate",phdi_mean="Climate",
                           L_SLOPE_DPTH="Morphometry",
                           asin_PCTFOR_WsRp100="Riparian cover",  asin_PCTWET_WsRp100="Riparian cover",LQbkf_kmcl="Hydrology", LQLow_kmcl="Hydrology",
                           d.excess_sc="Hydrology", Lpt01_XCMGW="Riparian cover", Lpt01_XFC_NAT="Habitat", LRBS_use="Habitat",
                           W1_HAG = "Riparian disturb", L_NTL="Chemistry",L_SULF="Chemistry")) #asin_PCTAGR_WS="landuse", Lpt01_XFC_NAT="Habitat",

teff$category <- ordered(teff$category, levels=c("Landuse","Climate","Hydrology","Morphometry","Riparian disturb","Riparian cover","Habitat","Chemistry"))
table(teff$category)

# RENAME RESPONSE
table(teff$response)
teff$response_2 <-teff$response
teff <- teff %>%
  mutate(response_2 = recode(response_2,
                             LQbkf_kmcl="bnk_f", LQLow_kmcl="low_f",
                             d.excess_sc="d.excess", Lpt01_XCMGW="r_cover",
                             Lpt01_XFC_NAT="instream_hab",LRBS_use="rbs",
                             L_NTL="TN",L_SULF="SO4", OE_SCORE="OE", MMI_BENT_sc="MMI",EPT_RICH_sc="EPT")) #Lpt01_XFC_NAT="strm_hab",

teff$response_2 <- ordered(teff$response_2, levels=c("OE","MMI","EPT","TN","SO4","rbs","instream_hab",
                                                     "r_cover","d.excess",
                                                     "bnk_f", "low_f")) # "strm_hab",
table(teff$response_2)

# ORDER EFFECT
teff$effect2 <-teff$effect
teff <- teff %>%
  mutate(effect2 = recode(effect2,
                          total="Total",
                          direct="Direct",
                          indirect="Indirect"))
teff$effect2 <- ordered(teff$effect2, levels=c("Total","Direct","Indirect"))
table(teff$effect2)

# ORDER BY RESPONSE AND DRIVER CLASS
teff<-teff[order(teff$response_2, teff$category),]


######################
## FIGURE OF PROPORTION OF TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
teff_total <- teff %>%
  filter(effect == "total")


#########################
## BARCHART CONUS LOW HYDRAP FOR INDIVIDUAL PREDICTORS
##########
# Individual plots by response
teff_total_all <- teff_total %>%
  filter(response_2=="OE"|response_2=="MMI"|response_2=="EPT") %>%
  droplevels()

#########################
## TOTAL EFFECTS on ALL BIOTIC INTEGRITY METRICS
ALL_WMT_w<-ggplot(teff_total_all ,aes(predictor_2,est_std,fill=category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  #ylim(-0.6,0.9)+
  facet_wrap(teff_total_all$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y = element_text(family="RMN"), #element_blank(),#
        strip.text.x = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+
  xlab(NULL)#+

ALL_WMT_w

################
## INDIVIDUAL PLOTS OF EFFECTS - Landscape orientation
# ALL BIOTIC INTEGRITY METRICS TOTAL
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_WMT_w_m9_ALL.tiff",
     width=9, height = 5, units="in", res=300)
ALL_WMT_w
dev.off()


######################
## FIGURE OF PROPORTION OF DIRECT EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY DIRECT EFFECTS
teff_direct <- teff %>%
  filter(effect == "direct")


#########################
## BARCHART CONUS LOW HYDRAP FOR INDIVIDUAL PREDICTORS
##########
# Individual plots by response
teff_direct_all <- teff_direct %>%
  filter(response_2=="OE"|response_2=="MMI"|response_2=="EPT") %>%
  droplevels()

#########################
## DIRECT EFFECTS on ALL BIOTIC INTEGRITY METRICS
ALL_WMT_w_direct<-ggplot(teff_direct_all ,aes(predictor_2,est_std,fill=category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid_d, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  #ylim(-0.6,0.9)+
  facet_wrap(teff_direct_all$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y = element_text(family="RMN"), #element_blank(),#
        strip.text.x = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Direct effects")+
  xlab(NULL)#+

ALL_WMT_w_direct

################
## INDIVIDUAL PLOTS OF EFFECTS - Landscape orientation
# ALL BIOTIC INTEGRITY METRICS DIRECT
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Direct_eff_WMT_w_m9_ALL.tiff",
     width=9, height = 5, units="in", res=300)
ALL_WMT_w_direct
dev.off()


######################
## FIGURE OF PROPORTION OF INDIRECT EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY INDIRECT EFFECTS
teff_indirect <- teff %>%
  filter(effect == "indirect")


#########################
## BARCHART CONUS LOW HYDRAP FOR INDIVIDUAL PREDICTORS
##########
# Individual plots by response
teff_indirect_all <- teff_indirect %>%
  filter(response_2=="OE"|response_2=="MMI"|response_2=="EPT") %>%
  droplevels()

#########################
## INDIECT EFFECTS on ALL BIOTIC INTEGRITY METRICS
ALL_WMT_w_indirect<-ggplot(teff_indirect_all ,aes(predictor_2,est_std,fill=category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid_i, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  #ylim(-0.6,0.9)+
  facet_wrap(teff_indirect_all$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y = element_text(family="RMN"), #element_blank(),#
        strip.text.x = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Indirect effects")+
  xlab(NULL)#+

ALL_WMT_w_indirect

################
## INDIVIDUAL PLOTS OF EFFECTS - Landscape orientation
# ALL BIOTIC INTEGRITY METRICS INDIRECT
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Indirect_eff_WMT_w_m9_ALL.tiff",
     width=9, height = 5, units="in", res=300)
ALL_WMT_w_indirect
dev.off()


##############################
## GRID OF ALL EFFECTS AND ALL BIOTIC RESPONSES
#########################
# SELECT BIOTIC INDEX RESPONSE
teff_bio <- teff %>%
  filter(response_2=="OE"|response_2=="MMI"|response_2=="EPT") %>%
  droplevels()

## INDIECT EFFECTS on ALL BIOTIC INTEGRITY METRICS
ALL_WMT_w_eff<-ggplot(teff_bio ,aes(predictor_2,est_std,fill=category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  facet_grid(vars(teff_bio$effect2),vars(teff_bio$response_2))+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y = element_text(family="RMN"), #element_blank(),#
        strip.text.x = element_text(family="RMN", size=12),
        strip.text.y = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Standardized effects")+
  xlab(NULL)#+

ALL_WMT_w_eff

################
## GRID OF EFFECTS - Landscape orientation
# ALL BIOTIC INTEGRITY METRICS TOTAL, DIRECT, & INDIRECT
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Effects_WMT_w_m9_ALL.tiff",
     width=9, height = 7.5, units="in", res=300)
ALL_WMT_w_eff
dev.off()



################################
################################
## TOTAL EFFECTS on BENTHIC MMI
MMI_WMT_w<-ggplot(teff_total_MMI ,aes(predictor_2,est_std,fill=category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  #ylim(-0.6,0.9)+
  facet_wrap(teff_total_MMI$effect2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y = element_text(family="RMN"), #element_blank(),#
        strip.text.x = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+
  xlab(NULL)#+

MMI_WMT_w

################
## INDIVIDUAL PLOTS OF EFFECTS
# TOTAL
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_WMT_w_m1_MMI.tiff",
     width=4.5, height = 4, units="in", res=200)
MMI_WMT_w
dev.off()

##############################
#############################
#############################
## EPT TOTAL EFFECTS WMT wadeable
## 5/10/22
#############################

#######################
## LOAD DATA
## WESTERN WADEABLE MODEL 1 EPT OUTPUT
# HAND COMPILED TOTAL EFFECTS (Standardized with Std error) from SEM model output with bootstrap sampling
teff_ept <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Total_effects/WMT_w_m1_EPT_tot.csv")

names(teff_ept)
table(teff_ept$predictor)

##############
## PROCESS DATA
# RELABEL PREDICTOR VARIABLES
teff_ept$predictor_2<-teff_ept$predictor
teff_ept <- teff_ept %>%
  mutate(predictor_2= recode(predictor_2,
                             asin_PCTAGR_WS="Agr Ws",asin_PCTURB_WS="Urban Ws", PSUMPY_SY_WS_sc="Precipitation",L_NABD_NrmStorWs_ratio="Dam",
                             asin_PCTFOR_WsRp100="Forest Rp",  asin_PCTWET_WsRp100="Wetland Rp",LQbkf_kmcl="Bankfull flow", LQLow_kmcl="Low flow",
                             d.excess_sc="d-excess", Lpt01_XCMGW="Riparian cover", LRBS_use="Bed stability",
                             W1_HAG = "Agriculture index", L_NTL="TN",L_SULF="Sulfate")) #Lpt01_XFC_NAT="Instream cover",

table(teff_ept$predictor_2)

# ORDER PREDICTOR VARIABLES
teff_ept$predictor_2 <- ordered(teff_ept$predictor_2, levels=c("Agr Ws","Urban Ws","Dam","Precipitation",
                                                       "Bankfull flow","Low flow","d-excess",
                                                       "Agriculture index",
                                                       "Forest Rp","Wetland Rp","Riparian cover",
                                                       "Bed stability","TN","Sulfate")) #"Instream cover","agr",
table(teff_ept$predictor_2)


# CREATE CATEGORIES
teff_ept$category <-teff_ept$predictor
teff_ept <- teff_ept %>%
  mutate(category = recode(category,
                           asin_PCTAGR_WS="Landuse",asin_PCTURB_WS="Landuse", PSUMPY_SY_WS_sc="Climate",L_NABD_NrmStorWs_ratio="Landuse",
                           asin_PCTFOR_WsRp100="Riparian cover",  asin_PCTWET_WsRp100="Riparian cover",LQbkf_kmcl="Hydrology", LQLow_kmcl="Hydrology",
                           d.excess_sc="Hydrology", Lpt01_XCMGW="Riparian cover",  LRBS_use="Habitat",
                           W1_HAG = "Riparian disturb", L_NTL="Chemistry",L_SULF="Chemistry")) #asin_PCTAGR_WS="landuse", Lpt01_XFC_NAT="Habitat",

teff_ept$category <- ordered(teff_ept$category, levels=c("Landuse","Climate","Hydrology","Riparian disturb","Riparian cover","Habitat","Chemistry"))
table(teff_ept$category)

# RENAME RESPONSE
table(teff_ept$response)
teff_ept$response_2 <-teff_ept$response
teff_ept <- teff_ept %>%
  mutate(response_2 = recode(response_2,
                             LQbkf_kmcl="bnk_f", LQLow_kmcl="low_f",
                             d.excess_sc="d.excess", Lpt01_XCMGW="r_cover",
                             LRBS_use="rbs",
                             L_NTL="TN",L_SULF="SO4", EPT_RICH_sc = "EPT")) #Lpt01_XFC_NAT="strm_hab",

teff_ept$response_2 <- ordered(teff_ept$response_2, levels=c("EPT","TN","SO4","rbs",
                                                     "r_cover","d.excess",
                                                     "bnk_f", "low_f")) # "strm_hab",
table(teff_ept$response_2)

# ORDER EFFECT
teff_ept$effect2 <-teff_ept$effect
teff_ept <- teff_ept %>%
  mutate(effect2 = recode(effect2,
                          total="Total",
                          direct="Direct",
                          indirect="Indirect"))
teff_ept$effect2 <- ordered(teff_ept$effect2, levels=c("Indirect","Direct","Total"))
table(teff_ept$effect2)

# ORDER BY RESPONSE AND DRIVER CLASS
teff_ept<-teff_ept[order(teff_ept$response_2, teff_ept$category),]


######################
## FIGURE OF PROPORTION OF TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
teff_ept_total <- teff_ept %>%
  filter(effect == "total")


#########################
## BARCHART CONUS LOW HYDRAP FOR INDIVIDUAL PREDICTORS
##########
# Individual plots by response
teff_total_EPT <- teff_ept_total %>%
  filter(response_2=="EPT") %>%
  droplevels()


## TOTAL EFFECTS on BENTHIC OE
EPT_WMT_w<-ggplot(teff_total_EPT ,aes(predictor_2,est_std,fill=category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  #ylim(-0.6,0.9)+
  facet_wrap(teff_total_EPT$effect2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y = element_text(family="RMN"), #element_blank(),#
        strip.text.x = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+
  xlab(NULL)#+

EPT_WMT_w

################
## INDIVIDUAL PLOTS OF EFFECTS
# TOTAL
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_WMT_w_m1_EPT.tiff",
     width=4.5, height = 4, units="in", res=200)
EPT_WMT_w
dev.off()
