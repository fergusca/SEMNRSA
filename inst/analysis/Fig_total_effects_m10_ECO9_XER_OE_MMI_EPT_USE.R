##################
## BARCHARTS OF TOTAL EFFECTS OF PREDICTORS
## RESPONSE XER ECO9: OE, MMI, EPT
## WADEABLE SITES
## Model v10 revised
## Autmoated method taking R output and getting total effects rather than manipulating data in excel

## 8/19/2022
## 9/10/2022
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
library(stringr)
#install.packages("RColorBrewer")
library(RColorBrewer)

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
vid_d <- c("#440154","#443983","#31688e","#21918c","#35b779","#90d743","#fde725")
vid_i <- c("#440154","#443983","#31688e","#21918c","#440154","#35b779")

#############
## READ IN MODEL OUTPUT - XER
# SEM Standardized OUTPUT
## OE
oe<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m10_OE_CI.csv")

## MMI
mmi<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m10_MMI_CI.csv")

## EPT
ept<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m10_EPT_CI.csv")


#######################
# Need to treat SEMNRSA like a package to be able to run function relabeling predictors
devtools::load_all()

# Call function sem_eff_tab to process the raw SEM output
# Selects Total, Direct, and Indirect Effects on OE
# And relabels predictor variables to more useful names
# O/E
oe_proc<- sem_eff_tab(oe)%>%
  mutate(Model="OE")
names(oe_proc)

# MMI
mmi_proc<-sem_eff_tab(mmi)%>%
  mutate(Model="MMI")

# EPT
ept_proc<-sem_eff_tab(ept)%>%
  mutate(Model="EPT")

#############################
# Combine datasets
teff<- bind_rows(oe_proc,mmi_proc,ept_proc)

names(teff)
table(teff$Predictor,teff$Model)

# ORDER PREDICTOR VARIABLES
teff$Predictor <- ordered(teff$Predictor, levels=c("Agr Ws","Urban Ws","Dam","Precipitation","Max temp","Drought index",
                                                   "Bankfull flow","Summer flow","Evaporation indicator","Slope*depth",
                                                   "Agr index Rp","Non-agr index Rp",
                                                   "Forest Rp","Wetland Rp","Riparian cover","Instream cover",
                                                   "Bed stability","TP","TN","Sulfate","Turbidity")) #"Instream cover","agr","SO4"
table(teff$Predictor)


# CREATE CATEGORIES
teff$Category <-teff$Predictor
teff <- teff %>%
  mutate(Category = recode_factor(Category,
                                  "Agr Ws"="Landuse","Urban Ws"="Landuse","Dam"="Landuse",
                                  "Max temp"="Climate","Precipitation"="Climate","Drought index"="Climate",
                                  "Slope*depth"="Morphometry",
                                  "Forest Rp"="Riparian cover",  "Wetland Rp"="Riparian cover","Bankfull flow"="Hydrology",
                                  "Summer flow"="Hydrology",
                                  "Evaporation indicator"="Hydrology", "Riparian cover"="Riparian cover", "Bed stability"="Habitat","Instream cover"="Habitat",
                                  "Agr index Rp" = "Riparian disturb", "Non-agr index Rp" = "Riparian disturb",
                                  "TN"="Chemistry","TP"="Chemistry","Sulfate"="Chemistry","Turbidity"="Chemistry")) #

teff$Category <- ordered(teff$Category, levels=c("Landuse","Climate","Hydrology","Morphometry","Riparian disturb","Riparian cover","Habitat","Chemistry"))
table(teff$Category)


# ORDER EFFECT
teff$Effects <- ordered(teff$Effects, levels=c("Total","Direct","Indirect"))
table(teff$Effects)

# ORDER BY RESPONSE AND DRIVER CLASS
teff$model<-ordered(teff$Model, levels=c("OE","MMI","EPT"))


######################
## FIGURE OF PROPORTION OF TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
teff_total <- teff %>%
  filter(Effects == "Total")


############################
## FIGURE WITH THREE PANELS: OE, MMI, EPT
#########################
## TOTAL EFFECTS
total_x<-ggplot(teff_total,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  #ylim(-0.6,0.9)+
  facet_wrap(~model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#31688e",3),rep("#21918c",1),rep("#440154",2),
                                            rep("#35b779",2),rep("#90d743",1),rep("#fde725",2))),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
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
  ggtitle("XER")


total_x

###################
# PRINT TOTAL EFFECTS
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_XERw_m10_OE_MMI_EPT.tiff",
     width=6, height = 8, units="in", res=300)
total
dev.off()


#####################
## XER O/E FOR SFS REGIONAL MEETING
##  10/4/22

oe_xer<-teff_total%>%
  filter(Model=="OE")

xer_oe<-ggplot(oe_xer,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  #ylim(-0.6,0.9)+
  facet_wrap(~model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",3), rep("#443983",2),rep("#31688e",3),rep("#21918c",1),rep("#440154",2),
                                            rep("#35b779",2),rep("#90d743",1),rep("#fde725",2))),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
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
  ggtitle("XER")

xer_oe

###################
# PRINT OE XER TOTAL EFFECTS FOR SFS PRESENTATION
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Tot_eff_ECO9_XERw_m10_OE_ONLY.tiff",
     width=5.5, height = 5, units="in", res=200)
xer_oe
dev.off()
