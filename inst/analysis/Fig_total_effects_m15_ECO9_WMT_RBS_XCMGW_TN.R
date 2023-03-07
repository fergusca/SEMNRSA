##################
## BARCHARTS OF TOTAL EFFECTS OF PREDICTORS
## RESPONSE WMT MMI: RBS, XCMGW, TN
## WADEABLE SITES
## Model v15
## Automated method taking R output and getting total effects rather than manipulating data in excel

## 1/24/2023
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
library(stringr)

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman
windowsFonts(CAL=windowsFont("Calibri"))
windowsFonts(AR=windowsFont("Arial"))

# SET COLOR PALATE FOR BY DRIVER CATEGORY

# Viridis Color palette
# https://waldyrious.net/viridis-palette-generator/
vid <- c("#440154","#443983","#31688e","#21918c","#440154","#35b779","#90d743","#fde725")
vid_x <- c("#440154","#443983","#21918c","#440154","#35b779")
vid_lf <- c("#440154","#443983","#440154","#35b779") #"#31688e","#21918c", ,"#90d743","#fde725"


vid_d <- c("#440154","#443983","#31688e","#21918c","#35b779","#90d743","#fde725")
vid_i <- c("#440154","#443983","#31688e","#21918c","#440154","#35b779")

#############
## READ IN MODEL OUTPUT MODEL 15
# SEM Standardized OUTPUT
## WMT MMI
mmi_w<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_MMI_CI_RBS_XCMGW_TN.csv")

## WMT EPT
ept_w<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_EPT_CI_SO4.csv")


## XER MMI
mmi_x<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_MMI_CI_RBS_XCMGW_TN.csv")

#######################
# Need to treat SEMNRSA like a package to be able to run function relabeling predictors
devtools::load_all()
library(SEMNRSA)

# Call function sem_eff_tab to process the raw SEM output
# Selects Total, Direct, and Indirect Effects on OE
# And relabels predictor variables to more useful names
# O/E
#oe_proc_x<- sem_eff_tab(oe_x)%>%
#  mutate(Model="OE")
#names(oe_proc_x)

# WMT MMI
mmi_proc_w<-sem_eff_tab(mmi_w)%>%
  mutate(Model="WMT")

# XER MMI
mmi_proc_x<-sem_eff_tab(mmi_x)%>%
  mutate(Model="XER")

table(mmi_proc_w$Predictor)
table(mmi_proc_x$Predictor)
table(mmi_proc_x$Effects)

# WMT EPT for SO4
ept_proc_w<-sem_eff_tab(ept_w)%>%
  mutate(Model="WMT")
table(ept_proc_w$Predictor)

#############################
# Combine datasets
teff<- bind_rows(mmi_proc_w,mmi_proc_x)

teff_sulf<- bind_rows(ept_proc_w,mmi_proc_x)

names(teff)
table(teff$Predictor,teff$Model)
table(teff$Effects)

# ORDER PREDICTOR VARIABLES
teff$Predictor <- ordered(teff$Predictor, levels=c("Agr Ws","Developed Ws","Dam","Precipitation","Max Temp","Drought index",
                                                       "Bankfull flow","Summer flow","Evaporation indicator","Slope*depth","Stream power",
                                                       "Agr index Rp","Non-agr index Rp",
                                                       "Forest Rp","Terrestrial cover Rp","Wetland Rp","Riparian site index","Instream cover",
                                                       "Bed stability","TP","TN","Sulfate","Turbidity")) #"Instream cover","agr","SO4"
table(teff$Predictor)


# CREATE CATEGORIES
teff$Category <-teff$Predictor
teff <- teff %>%
  mutate(Category = recode_factor(Category,
                                  "Agr Ws"="Land use","Developed Ws"="Land use","Dam"="Land use",
                                  "Max Temp"="Climate","Precipitation"="Climate","Drought index"="Climate",
                                  "Slope*depth"="Morphometry","Stream power"="Morphometry",
                                  "Forest Rp"="Riparian cover",  "Wetland Rp"="Riparian cover","Terrestrial cover Rp"="Riparian cover",
                                  "Bankfull flow"="Hydrology","Summer flow"="Hydrology",
                                  "Evaporation indicator"="Hydrology", "Riparian site index"="Riparian cover", "Bed stability"="Habitat","Instream cover"="Habitat",
                                  "Agr index Rp" = "Riparian land use", "Non-agr index Rp" = "Riparian land use",
                                  "TN"="Chemistry","TP"="Chemistry","Sulfate"="Chemistry","Turbidity"="Chemistry")) #

teff$Category <- ordered(teff$Category, levels=c("Land use","Climate","Hydrology","Morphometry","Riparian land use","Riparian cover","Habitat","Chemistry"))
table(teff$Category)

###################
# FOR SULFATE
# ORDER PREDICTOR VARIABLES
teff_sulf$Predictor <- ordered(teff_sulf$Predictor, levels=c("Agr Ws","Developed Ws","Dam","Precipitation","Max Temp","Drought index",
                                                   "Bankfull flow","Summer flow","Evaporation indicator","Slope*depth","Stream power",
                                                   "Agr index Rp","Non-agr index Rp",
                                                   "Forest Rp","Terrestrial cover Rp","Wetland Rp","Riparian site index","Instream cover",
                                                   "Bed stability","TP","TN","Sulfate","Turbidity")) #"Instream cover","agr","SO4"
table(teff_sulf$Predictor)


# CREATE CATEGORIES
teff_sulf$Category <-teff_sulf$Predictor
teff_sulf <- teff_sulf %>%
  mutate(Category = recode_factor(Category,
                                  "Agr Ws"="Land use","Developed Ws"="Land use","Dam"="Land use",
                                  "Max Temp"="Climate","Precipitation"="Climate","Drought index"="Climate",
                                  "Slope*depth"="Morphometry","Stream power"="Morphometry",
                                  "Forest Rp"="Riparian cover",  "Wetland Rp"="Riparian cover","Terrestrial cover Rp"="Riparian cover",
                                  "Bankfull flow"="Hydrology","Summer flow"="Hydrology",
                                  "Evaporation indicator"="Hydrology", "Riparian site index"="Riparian cover", "Bed stability"="Habitat","Instream cover"="Habitat",
                                  "Agr index Rp" = "Riparian land use", "Non-agr index Rp" = "Riparian land use",
                                  "TN"="Chemistry","TP"="Chemistry","Sulfate"="Chemistry","Turbidity"="Chemistry")) #

teff_sulf$Category <- ordered(teff_sulf$Category, levels=c("Land use","Climate","Hydrology","Morphometry","Riparian land use","Riparian cover","Habitat","Chemistry"))
table(teff_sulf$Category)




######################
## FIGURE OF PROPORTION OF TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
table(teff$Effects)

teff_rbs <- teff %>%
  filter(Effects == "Total RBS")

teff_xcmgw <- teff %>%
  filter(Effects == "Total XCMGW")

teff_tn <- teff %>%
  filter(Effects == "Total TN")

#teff_sulf <- teff %>%
#  filter(Effects == "Total SO4")

teff_sulf_ept <-teff_sulf %>%
  filter(Effects == "Total SO4")

teff_evap <- teff %>%
  filter(Effects == "Total Evap")

teff_lflow <- teff %>%
  filter(Effects == "Total Low flow")

teff_bflow <- teff %>%
  filter(Effects == "Total Bankfull flow")

teff_sp <- teff %>%
  filter(Effects == "Total Specific power")


############################
## FIGURE WITH TWO PANELS: WMT, XER
#########################
## RBS TOTAL EFFECTS
total_rbs<-ggplot(teff_rbs,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.60,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#31688e",2),rep("#21918c",1),rep("#440154",2),
                                            rep("#35b779",3),rep("#90d743",1),rep("#fde725",2))),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_blank(),#element_text(family="AR"), #
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=12))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("RBS")


total_rbs


##############
## XCMGW TOTAL EFFECTS
total_xcmgw<-ggplot(teff_xcmgw,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid_x, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.60,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#21918c",1),rep("#440154",2), # flow rep("#31688e",2),
                                            rep("#35b779",2))),#, rbs rep("#90d743",1),chemistry rep("#fde725",2)
        axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_text(family="AR"), #element_blank(),#
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("XCMGW")

total_xcmgw

##############
## TN TOTAL EFFECTS
total_tn<-ggplot(teff_tn,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.55,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#31688e",3),rep("#21918c",1),rep("#440154",2), # flow rep("#31688e",2),
                                            rep("#35b779",3))),#, rbs rep("#90d743",1),chemistry rep("#fde725",2)
        axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_text(family="AR"), #element_blank(),#
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("TN")

total_tn

##################
## SO4 TOTAL EFFECTS
total_sulf_ept<-ggplot(teff_sulf_ept,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.55,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#31688e",3),rep("#21918c",1),rep("#440154",2), # flow rep("#31688e",2),
                                            rep("#35b779",3))),#, rbs rep("#90d743",1),chemistry rep("#fde725",2)
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_blank(),#element_text(family="AR"), #
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("Sulfate")

total_sulf_ept

#####################
## Evaporation TOTAL EFFECTS
total_evap<-ggplot(teff_evap,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.60,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#31688e",2),rep("#21918c",1),rep("#440154",2), # , flow ,
                                            rep("#35b779",2))),#, rbs rep("#90d743",1),chemistry rep("#fde725",2)
        axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_text(family="AR"), #element_blank(),#
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("Evaporation indicator")

total_evap

#####################
## Summer low flow TOTAL EFFECTS
total_lflow<-ggplot(teff_lflow,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid_lf, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.60,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#440154",2), # sp power rep("#21918c",1), flow rep("#31688e",2),
                                            rep("#35b779",2))),#, rbs rep("#90d743",1),chemistry rep("#fde725",2)
        axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_text(family="AR"), #element_blank(),#
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("Summer low flow")

total_lflow

#####################
## Bankfull flow TOTAL EFFECTS
total_bflow<-ggplot(teff_bflow,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid_lf, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.7,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",2), rep("#443983",2),rep("#440154",2), # flow rep("#31688e",2),
                                            rep("#35b779",2))),#, rbs rep("#90d743",1),chemistry rep("#fde725",2)
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_blank(),# element_text(family="AR"), #
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("Bankfull flow")

total_bflow

#####################
## Specific stream power TOTAL EFFECTS
total_sp<-ggplot(teff_sp,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid_lf, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.60,0.55)+
  facet_wrap(~Model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",1), rep("#443983",1),rep("#440154",1), # rep("#21918c",1), flow rep("#31688e",2),
                                            rep("#35b779",2))),#, rbs rep("#90d743",1),chemistry rep("#fde725",2)
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_blank(),#element_text(family="AR"), #
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("Specific stream power")

total_sp


###################
# PRINT TOTAL EFFECTS CHARTS INDIVIDUALLY
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_WMTXERw_m15_XCMGW.tiff",
     width=4, height = 6, units="in", res=300)
total_xcmgw
dev.off()




#########################
## COMBINE WMT AND XER - run XER Fig code
# GET LEGEND
legend <- get_legend(total_rbs)

# REMOVE LEGEND
total_rbs <- total_rbs + theme(legend.position="none")

# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
# XCMGW + RBS
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_WMTXERw_m15_XCMGW_RBS.tiff",
     width=8, height=6, units="in", res=300)
grid.arrange(arrangeGrob(total_xcmgw,
                         total_rbs,
                         ncol=2,widths=c(4,3.5)),
             legend,nrow=2,heights=c(8, .5))
dev.off()


################
## TN + SULFATE
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_WMTXERw_m15_TN_SO4.tiff",
     width=8, height=6, units="in", res=300)
grid.arrange(arrangeGrob(total_tn,
                         total_sulf_ept,
                         ncol=2,widths=c(4,3.5)),
             legend,nrow=2,heights=c(8, .5))
dev.off()

# Create an empty graph as a place holder in grid
#empty<-ggplot(data.frame(x=1, y=1, colour='Something'), aes(x,y,fill=colour)) +
#  geom_point(alpha=0, shape=0) +
#  scale_fill_manual(values='white',drop=FALSE) +
#  theme(axis.title=element_blank(),
#        axis.text=element_blank(),
#        axis.ticks=element_blank(),
#        legend.position="none",
#        panel.grid=element_blank(),
#        panel.border=element_rect(color="white",fill='white', size=1))

#tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_WMTXERw_m15_TN_SO4.tiff",
#     width=8, height=6, units="in", res=300)
#grid.arrange(total_tn,
#             arrangeGrob(empty,
#                         total_sulf,
#                         ncol=1,nrow=2,heights=c(1.8,3)), #widths=c(4,3.5)),#, heights=c(8,4,.5)))#,
#             legend,nrow=2,heights=c(8, .5),widths=c(4,3.5))
#dev.off()

######################
# Evap and specific power
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_WMTXERw_m15_EVAP_PWR.tiff",
     width=8, height=6, units="in", res=300)
grid.arrange(arrangeGrob(total_evap,
                         total_sp,
                         ncol=2,widths=c(4,3.5)),
             legend,nrow=2,heights=c(8, .5))
dev.off()


######################
# Summer low flow and bankfull flow
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_WMTXERw_m15_LFLOW_BFLOW.tiff",
     width=8, height=6, units="in", res=300)
grid.arrange(arrangeGrob(total_lflow,
                         total_bflow,
                         ncol=2,widths=c(4,3.5)),
             legend,nrow=2,heights=c(8, .5))
dev.off()
