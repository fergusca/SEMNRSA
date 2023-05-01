############################
## MODEL 15 - REVISED to change arrow direction to stream specific power
#                 from discharge
## PATH MODELS FOR WESTERN MOUNTAINS
## PREDICTING OE, MMI, & EPT
##
## 12/1/2022
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


#####################
## REVISED WMT MMI
mymodel_v15_MMI_tot <- '
Lpt01_XCMGW ~ ha3*W1_HAG + sp3*L_STRM_POWER + p3*PSUMPY_SY_WS_sc
L_STRM_POWER ~ d2*L_NABD_NrmStorWs_ratio + ha4*W1_HAG + rn2*asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl~ u4*asin_PCTURB_WS + p4*PSUMPY_SY_WS_sc + rn3*asin_PCTNATTERR_WsRp100 + ha5*W1_HAG + ph2*drought_mean
LQbkf_kmcl ~ p5*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + ha6*W1_HAG
evap_index_sc ~ l3*LQLow_kmcl + b2*LQbkf_kmcl + p6*PSUMPY_SY_WS_sc + d4*L_NABD_NrmStorWs_ratio + rn4*asin_PCTNATTERR_WsRp100
LRBS_use ~ u2*asin_PCTURB_WS + x2*Lpt01_XCMGW + ha2*W1_HAG + p2*PSUMPY_SY_WS_sc + sp2*L_STRM_POWER
L_NTL ~ u3*asin_PCTURB_WS + x3*Lpt01_XCMGW + e2*evap_index_sc + w2*asin_PCTWET_WsRp100 + l2*LQLow_kmcl

MMI_BENT_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + b1*LQbkf_kmcl

# INDIRECT EFFECTS ON MMI
urb_mmi:= u2*r1 + u3*n1 + u4*l2*n1 + u4*l3*e2*n1
#agr_mmi:=
dam_mmi:= d2*sp2*r1 + d2*sp3*x1 + d2*sp3*x2*r1 + d2*sp3*x3*n1 + d3*b1 + d3*b2*e2*n1 + d4*e2*n1
precip_mmi:= p2*r1 + p3*x1 + p3*x2*r1 + p3*x3*n1 + p4*l2*n1 + p4*l3*e2*n1 + p5*b1 + p5*b2*e2*n1 + p6*e2*n1
phdi_mmi:= ph2*l2*n1 + ph2*l3*e2*n1
strpwr_mmi:= sp2*r1 + sp3*x1 + sp3*x2*r1 + sp3*x3*n1
hag_mmi:= ha2*r1 + ha3*x1 + ha3*x2*r1 + ha3*x3*n1 + ha4*sp2*r1 + ha4*sp3*x1 + ha4*sp3*x2*r1 + ha4*sp3*x3*n1 + ha5*l2*n1 + ha5*l3*e2*n1 + ha6*b1 + ha6*b2*e2*n1
#nhag_mmi:=
rnat_mmi:= rn2*sp2*r1 + rn2*sp3*x1 + rn2*sp3*x2*r1 + rn2*sp3*x3*n1 + rn3*l2*n1 +rn3*l3*e2*n1 + rn4*e2*n1
rwet_mmi:= w2*n1
lflow_mmi:= l2*n1 + l3*e2*n1 #
bflow_mmi:= b2*e2*n1
dexcess_mmi:= e2*n1
xcmgw_mmi:= x2*r1 + x3*n1

# TOTAL EFFECTS ON MMI
urb_tot:= urb_mmi
dam_tot:= dam_mmi
precip_tot:= precip_mmi
phdi_tot:= phdi_mmi
strpwr_tot:= strpwr_mmi
hag_tot:= hag_mmi
#nhag_tot:= nhag_mmi
rnat_tot:= rnat_mmi #rn1 +
rwet_tot:= rwet_mmi #w1 +
lflow_tot:= lflow_mmi
bflow_tot:= b1 + bflow_mmi
dexcess_tot:= dexcess_mmi
xcmgw_tot:= x1 + xcmgw_mmi
rbs_tot:= r1
tn_tot:= n1

# TOTAL EFFECTS ON RBS
urb_rbstot:= u2
dam_rbstot:= d2*sp2 + d2*sp3*x2
precip_rbstot:= p2 + p3*x2
strpwr_rbstot:= sp2 + sp3*x2
hag_rbstot:= ha2 + ha3*x2 + ha4*sp2 + ha4*sp3*x2
rnat_rbstot:= rn2*sp2 + rn2*sp3*x2
xcmgw_rbstot:= x2

# TOTAL EFFECTS ON XCMGW
#urb_xcmgwtot:=
dam_xcmgwtot:= d2*sp3
precip_xcmgwtot:= p3
strpwr_xcmgwtot:= sp3
hag_xcmgwtot:= ha3 + ha4*sp3
rnat_xcmgwtot:= rn2*sp3

# TOTAL EFFECTS ON TN
urb_tntot:= u3 + u4*l2 + u4*l3*e2
dam_tntot:= d2*sp3*x3 + d3*b2*e2 + d4*e2
precip_tntot:= p3*x3 + p4*l2 + p4*l3*e2 + p5*b2*e2 + p6*e2
phdi_tntot:= ph2*l2 + ph2*l3*e2
strpwr_tntot:= sp3*x3
hag_tntot:= ha3*x3 + ha4*sp3*x3 + ha5*l2 + ha5*l3*e2 + ha6*b2*e2
rnat_tntot:= rn2*sp3*x3 + rn3*l2 + rn3*l3*e2 + rn4*e2
rwet_tntot:= w2
lflow_tntot:= l2 + l3*e2
bflow_tntot:= b2*e2
dexcess_tntot:= e2
xcmgw_tntot:= x3

# TOTAL EFFECTS ON EVAPORATION INDICATOR
urb_evtot:= u4*l3
dam_evtot:= d4 + d3*b2
precip_evtot:= p6 + p4*l3 + p5*b2
phdi_evtot:= ph2*l3
#strpwr_evtot:= strpwr_mmi
hag_evtot:= ha5*l3 + ha6*b2
rnat_evtot:= rn4 + rn3*l3
lflow_evtot:= l3
bflow_evtot:= b2

# TOTAL EFFECTS ON SUMMER FLOW
urb_lftot:= u4
#dam_lftot:= dam_mmi
precip_lftot:= p4
phdi_lftot:= ph2
#strpwr_lftot:= strpwr_mmi
hag_lftot:= ha5
#nhag_lftot:= nhag_mmi
rnat_lftot:= rn3

# TOTAL EFFECTS ON BANKFULL FLOW
dam_bftot:= d3
precip_bftot:= p5
hag_bftot:= ha6

# TOTAL EFFECTS ON SP STRM POWER
dam_sptot:= d2
hag_sptot:= ha4
rnat_sptot:= rn2

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl

'

# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_WMTw_MMI_tot_robust.est<- sem(mymodel_v15_MMI_tot, data=wmt_w,
                                      estimator="MLM")

summary(fit_v15_WMTw_MMI_tot_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_WMTw_MMI_tot_robust.est)
print(mi_min[mi_min$mi >3.0,])


######################
## Bollen.stine bootstrap to estimate parameters
fit_v15_WMTw_MMI_bootstrap_tot  <- sem(mymodel_v15_MMI_tot, data=wmt_w,
                                       #group = "ECOREG_rev",
                                       #missing="ML",
                                       test="bollen.stine", se="boot",bootstrap=1000)

summary(fit_v15_WMTw_MMI_bootstrap_tot, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_WMTw_MMI_bootstrap_tot)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - MMI MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v15_WMTw_MMI_tot<- capture.output(summary(fit_v15_WMTw_MMI_bootstrap_tot, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v15_WMTw_MMI_tot, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_MMI_RBS_XCMGW_TN.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min_tot<- standardizedSolution(fit_v15_WMTw_MMI_bootstrap_tot)
write.csv(std_parameter_se_bootstrap_min_tot, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_MMI_CI_RBS_XCMGW_TN.csv",
          row.names = FALSE)


#######################################
#######################################
#####################
## REVISED WMT EPT FOR SULFATE DRIVERS
mymodel_v15_EPT_tot <- '
Lpt01_XCMGW ~ ha3*W1_HAG + sp3*L_STRM_POWER + p3*PSUMPY_SY_WS_sc
L_STRM_POWER ~ d2*L_NABD_NrmStorWs_ratio + ha4*W1_HAG + rn2*asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl~ u3*asin_PCTURB_WS + p4*PSUMPY_SY_WS_sc + rn3*asin_PCTNATTERR_WsRp100 + ha5*W1_HAG + ph2*drought_mean
LQbkf_kmcl ~ p6*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + ha6*W1_HAG
evap_index_sc ~  l2*LQLow_kmcl + b3*LQbkf_kmcl + p7*PSUMPY_SY_WS_sc + d4*L_NABD_NrmStorWs_ratio + rn4*asin_PCTNATTERR_WsRp100
LRBS_use ~ u2*asin_PCTURB_WS + x2*Lpt01_XCMGW + ha2*W1_HAG + p2*PSUMPY_SY_WS_sc + sp2*L_STRM_POWER
L_NTL ~ u4*asin_PCTURB_WS + x3*Lpt01_XCMGW + e2*evap_index_sc + w2*asin_PCTWET_WsRp100
L_SULF ~ b2*LQbkf_kmcl + p5*PSUMPY_SY_WS_sc

EPT_RICH_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + su1*L_SULF + b1*LQbkf_kmcl + rn1*asin_PCTNATTERR_WsRp100

# INDIRECT EFFECTS ON EPT
urb_ept:= u2*r1 + u3*l2*e2*n1 + u4*n1
#agr_ept:=
dam_ept:= d2*sp2*r1 + d2*sp3*x1 + d2*sp3*x2*r1 + d2*sp3*x3*n1 + d3*b1 + d3*b2*su1 + d3*b3*e2*n1 + d4*e2*n1
precip_ept:= p2*r1 + p3*x1 + p3*x2*r1 + p3*x3*n1 +p4*l2*e2*n1 + p5*su1 + p6*b1 + p6*b2*su1 + p6*b3*e2*n1 + p7*e2*n1
phdi_ept:= ph2*l2*e2*n1
strpwr_ept:= sp2*r1 + sp3*x1 + sp3*x2*r1 + sp3*x3*n1
hag_ept:= ha2*r1 + ha3*x1 + ha3*x2*r1 + ha3*x3*n1 + ha4*sp2*r1 + ha4*sp3*x1 + ha4*sp3*x2*r1 + ha4*sp3*x3*n1 + ha5*l2*e2*n1 + ha6*b1 + ha6*b2*su1 + ha6*b3*e2*su1
#nhag_ept:=
rnat_ept:= rn2*sp2*r1 + rn2*sp3*x1 + rn2*sp3*x2*r1 + rn2*sp3*x3*n1 + rn3*l2*e2*n1 + rn4*e2*n1
rwet_ept:= w2*n1
lflow_ept:= l2*e2*n1 #
bflow_ept:= b2*su1 + b3*e2*n1
dexcess_ept:= e2*n1
xcmgw_ept:= x2*r1 + x3*n1

# TOTAL EFFECTS ON EPT
urb_tot:= urb_ept
dam_tot:= dam_ept
precip_tot:= precip_ept
phdi_tot:= phdi_ept
strpwr_tot:= strpwr_ept
hag_tot:= hag_ept
#nhag_tot:= nhag_ept
rnat_tot:= rn1 + rnat_ept #
rwet_tot:= rwet_ept
lflow_tot:= lflow_ept
bflow_tot:= b1 + bflow_ept
dexcess_tot:= dexcess_ept
xcmgw_tot:= x1 + xcmgw_ept
rbs_tot:= r1
tn_tot:= n1
sulf_tot:= su1

# TOTAL EFFECTS ON SULFATE
dam_sulftot:= d3*b2
precip_sulftot:= p5 + p6*b2
hag_sulftot:= ha6*b2
bflow_sulftot:= b2


# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_NTL ~~                 L_SULF

'

# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_WMTw_EPT_tot_robust.est<- sem(mymodel_v15_EPT_tot, data=wmt_w,
                                      estimator="MLM")

summary(fit_v15_WMTw_EPT_tot_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_WMTw_EPT_tot_robust.est)
print(mi_min[mi_min$mi >3.0,])


######################
## Bollen.stine bootstrap to estimate parameters -OE
fit_v15_WMTw_EPT_bootstrap_tot  <- sem(mymodel_v15_EPT_tot, data=wmt_w,
                                       #group = "ECOREG_rev",
                                       #missing="ML",
                                       test="bollen.stine", se="boot",bootstrap=1000)

summary(fit_v15_WMTw_EPT_bootstrap_tot, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_WMTw_EPT_bootstrap_tot)
print(mi_min[mi_min$mi >3.0,])


#############
# Export R output - EPT MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v15_WMTw_EPT_tot<- capture.output(summary(fit_v15_WMTw_EPT_bootstrap_tot, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v15_WMTw_EPT_tot, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_EPT_SO4.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_tot<- standardizedSolution(fit_v15_WMTw_EPT_bootstrap_tot)
write.csv(std_parameter_se_bootstrap_tot, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_EPT_CI_SO4.csv",
          row.names = FALSE)
