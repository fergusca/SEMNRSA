############################
## MODEL 15 - REVISED to change arrow direction to stream specific power
#                 from discharge
## PATH MODELS FOR XER
## PREDICTING OE, MMI, & EPT
##
## 1/24/2023 - added total effects on RBS, XCMGW, TN, and SO4 for MMI/EPT models
###########################

remove(list=ls())

library(tidyverse)
library(lavaan)
library(lavaanPlot)
#library(sem)
library(psych)
library(dplyr)
#library(semPlot)
library(DiagrammeRsvg)
library(rsvg)
library(ggplot2)

#library(GGally)

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



#####################
## REVISED XER MMI
mymodel_v15_MMI_tot <- '
Lpt01_XCMGW ~ a2*asin_PCTAGR_WS + u2*asin_PCTURB_WS + ha3*W1_HAG + na2*W1_HNOAG + sp2*L_STRM_POWER
L_STRM_POWER ~ ph2*drought_mean + d2*L_NABD_NrmStorWs_ratio + ha4*W1_HAG + rn2*asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl~ a3*asin_PCTAGR_WS + p3*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 + ha5*W1_HAG + na5*W1_HNOAG
LQbkf_kmcl ~ u4*asin_PCTURB_WS + p5*PSUMPY_SY_WS_sc + d4*L_NABD_NrmStorWs_ratio +  ha6*W1_HAG + ph3*drought_mean
evap_index_sc ~ l5*LQLow_kmcl + b3*LQbkf_kmcl + p7*PSUMPY_SY_WS_sc + rn3*asin_PCTNATTERR_WsRp100 + ph5*drought_mean + sp3*L_STRM_POWER
LRBS_use ~ l2*LQLow_kmcl + b2*LQbkf_kmcl + ha2*W1_HAG + p2*PSUMPY_SY_WS_sc
L_NTL ~ u3*asin_PCTURB_WS + na3*W1_HNOAG + l3*LQLow_kmcl + p4*PSUMPY_SY_WS_sc
L_SULF ~ u5*asin_PCTURB_WS + e2*evap_index_sc + na4*W1_HNOAG + l4*LQLow_kmcl + p6*PSUMPY_SY_WS_sc + ph4*drought_mean

MMI_BENT_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + su1*L_SULF + na1*W1_HNOAG + ph1*drought_mean

# INDIRECT EFFECTS ON MMI
urb_mmi:= u2*x1 + u3*n1 + u4*b2*r1 + u4*b3*e2*su1 + u5*su1
agr_mmi:= a2*x1 + a3*l2*r1 + a3*l3*n1 + a3*l4*su1 + a3*l5*e2*su1
dam_mmi:= d2*sp2*x1 + d2*sp3*e2*su1 + d3*l2*r1 + d3*l3*n1 + d3*l4*su1 + d3*l5*e2*su1 + d4*b2*r1 + d4*b3*e2*su1
precip_mmi:= p2*r1 + p3*l2*r1 + p3*l3*n1 + p3*l4*su1 + p3*l5*e2*su1 + p4*n1 + p5*b2*r1 + p5*b3*e2*su1 + p6*su1 + p7*e2*su1
phdi_mmi:= ph2*sp2*x1 + ph2*sp3*e2*su1 + ph3*b2*r1 + ph3*b3*e2*su1 + ph4*su1 + ph5*e2*su1
strpwr_mmi:= sp2*x1 + sp3*e2*su1
hag_mmi:= ha2*r1 + ha3*x1 + ha4*sp2*x1 + ha4*sp3*e2*su1 + ha5*l2*r1 + ha5*l3*n1 + ha5*l4*su1 + ha5*l5*e2*su1 + ha6*b2*r1 + ha6*b3*e2*su1
nhag_mmi:= na2*x1 + na3*n1 + na4*su1 + na5*l2*r1 + na5*l3*n1 + na5*l4*su1 + na5*l5*e2*su1
rnat_mmi:= rn2*sp2*r1 + rn2*sp3*e2*su1 + rn3*e2*su1
rwet_mmi:= w2*l2*r1 + w2*l3*n1 + w2*l4*su1 + w2*l5*e2*su1
lflow_mmi:= l2*r1 + l3*n1 + l4*su1 + l5*e2*su1
bflow_mmi:= b2*r1 + b3*e2*su1
dexcess_mmi:= e2*su1
#xcmgw_mmi:=

# TOTAL EFFECTS ON MMI
urb_tot:= urb_mmi
agr_tot:= agr_mmi
dam_tot:= dam_mmi
precip_tot:= precip_mmi
phdi_tot:= ph1 + phdi_mmi
strpwr_tot:= strpwr_mmi
hag_tot:= hag_mmi
nhag_tot:= na1 + nhag_mmi
rnat_tot:= rnat_mmi #rn1 +
rwet_tot:= rwet_mmi #w1 +
lflow_tot:= lflow_mmi
bflow_tot:= bflow_mmi
dexcess_tot:= dexcess_mmi
xcmgw_tot:= x1
rbs_tot:= r1
tn_tot:= n1
sulf_tot:= su1

# TOTAL EFFECTS RBS
urb_rbstot:= u4*b2
agr_rbstot:= a3*l2
dam_rbstot:= d3*l2 + d4*b2
precip_rbstot:= p2+ p3*l2 + p5*b2
phdi_rbstot:= ph3*b2
#strpwr_rbstot:= strpwr_mmi
hag_rbstot:= ha2 + ha5*l2 + ha6*b2
nhag_rbstot:= na5*l2
#rnat_rbstot:=
rwet_rbstot:= w2*l2
lflow_rbstot:= l2
bflow_rbstot:= b2
#dexcess_rbstot:= dexcess_mmi
#xcmgw_rbstot:= x1

# TOTAL EFFECTS XCMGW
urb_xcmgwtot:= u2
agr_xcmgwtot:= a2
dam_xcmgwtot:= d2*sp2
phdi_xcmgwtot:= ph2*sp2
strpwr_xcmgwtot:= sp2
hag_xcmgwtot:= ha3 + ha4*sp2
nhag_xcmgwtot:= na2
rnat_xcmgwtot:= rn2*sp2

# TOTAL EFFECTS TN
urb_tntot:= u3
agr_tntot:= a3*l3
dam_tntot:= d3*l3
precip_tntot:= p4 + p3*l3
hag_tntot:= ha5*l3
nhag_tntot:= na3 + na5*l3
rwet_tntot:= w2*l3
lflow_tntot:= l3

# TOTAL EFFECTS SULFATE
urb_sulftot:= u5 + u4*b3*e2
agr_sulftot:= a3*l4 + a3*l5*e2
dam_sulftot:= d2*sp3*e2 + d3*l4 + d3*l5*e2 + d4*b3*e2
precip_sulftot:= p6 + p3*l4 + p3*l5*e2 + p5*b3*e2 + p7*e2
phdi_sulftot:= ph4 + ph2*sp3*e2 + ph3*b3*e2 + ph5*e2
strpwr_sulftot:= sp3*e2
hag_sulftot:= ha4*sp3*e2 + ha5*l4 + ha5*l5*e2 + ha6*b3*e2
nhag_sulftot:= na4 + na5*l4 + na5*l5*e2
rnat_sulftot:= rn2*sp3*e2 + rn3*e2
rwet_sulftot:= w2*l4 + w2*l5*e2
lflow_sulftot:= l4 + l5*e2
bflow_sulftot:= b3*e2
dexcess_sulftot:= e2


# TOTAL EFFECTS ON EVAPORATION INDICATOR
urb_evtot:= u4*b3
agr_evtot:= a3*l5
dam_evtot:= d2*sp3 + d3*l5 + d4*b3
precip_evtot:= p7 + p3*l5 + p5*b3
phdi_evtot:= ph5 + ph2*sp3 + ph3*b3
strpwr_evtot:= sp3
hag_evtot:= ha4*sp3 + ha5*l5 + ha6*b3
nhag_evtot:= na5*l5
rnat_evtot:= rn3 + rn2*sp3
rwet_evtot:= w2*l5
lflow_evtot:= l5
bflow_evtot:= b3

# TOTAL EFFECTS ON SUMMER FLOW
#urb_lftot:=
agr_lftot:= a3
dam_lftot:= d3
precip_lftot:= p3
#phdi_lftot:= ph2
hag_lftot:= ha5
nhag_lftot:= na5
#rnat_lftot:= rn3
rwet_lftot:= w2

# TOTAL EFFECTS ON BANKFULL FLOW
urb_bftot:= u4
dam_bftot:= d4
precip_bftot:= p5
phdi_bftot:= ph3
hag_bftot:= ha6

# TOTAL EFFECTS ON SP STRM POWER
dam_sptot:= d2
phdi_sptot:= ph2
hag_sptot:= ha4
rnat_sptot:= rn2


# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_NTL ~~                  L_SULF

'
# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_XERw_MMI_tot_robust.est<- sem(mymodel_v15_MMI_tot, data=xer_w,
                                      estimator="MLM")

summary(fit_v15_XERw_MMI_tot_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_MMI_tot_robust.est)
print(mi_min[mi_min$mi >3.0,])


######################
## Bollen.stine bootstrap to estimate parameters
fit_v15_XERw_MMI_bootstrap_tot  <- sem(mymodel_v15_MMI_tot, data=xer_w,
                                       #group = "ECOREG_rev",
                                       #missing="ML",
                                       test="bollen.stine", se="boot",bootstrap=1000)

summary(fit_v15_XERw_MMI_bootstrap_tot, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_MMI_bootstrap_tot)
print(mi_min[mi_min$mi >3.0,])


#############
# Export R output - MMI MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v15_XERw_MMI_tot<- capture.output(summary(fit_v15_XERw_MMI_bootstrap_tot, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v15_XERw_MMI_tot, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_MMI_RBS_XCMGW_TN.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min_tot<- standardizedSolution(fit_v15_XERw_MMI_bootstrap_tot)
write.csv(std_parameter_se_bootstrap_min_tot, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_MMI_CI_RBS_XCMGW_TN.csv",
          row.names = FALSE)

##################
