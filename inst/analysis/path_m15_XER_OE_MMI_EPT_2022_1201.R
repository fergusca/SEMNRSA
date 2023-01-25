############################
## MODEL 15 - REVISED to change arrow direction to stream specific power
#                 from discharge
## PATH MODELS FOR XERIC ARID ECOREGION
## PREDICTING OE, MMI, & EPT
##
## 12/1/2022
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


########################
## XERIC WADEABLE O/E
# REVISED MODEL dropping certain predictors
## Stream specific power and natural cover
mymodel_v15_OE_xer <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_STRM_POWER + PSUMPY_SY_WS_sc + drought_mean
L_STRM_POWER ~PSUMPY_SY_WS_sc + drought_mean + L_NABD_NrmStorWs_ratio + W1_HAG + asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + drought_mean
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + drought_mean
evap_index_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + drought_mean + L_STRM_POWER
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl + LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_STRM_POWER + drought_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean

OE_SCORE ~ Lpt01_XCMGW + LRBS_use + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_STRM_POWER + drought_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF
L_PTL ~~ L_TURB

'


##############
## XER O/E
# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_XERw_OE_robust.est<- sem(mymodel_v15_OE_xer, data=xer_w,
                                 estimator="MLM")

summary(fit_v15_XERw_OE_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_OE_robust.est)
print(mi_min[mi_min$mi >3.0,])

#####################
## REVISED
mymodel_v15_OE_rev_xer <- '
L_STRM_POWER ~ ph2*drought_mean + d2*L_NABD_NrmStorWs_ratio + ha3*W1_HAG + rn2*asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl~ a2*asin_PCTAGR_WS + p3*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 + ha4*W1_HAG + na4*W1_HNOAG
LQbkf_kmcl ~ u3*asin_PCTURB_WS + p4*PSUMPY_SY_WS_sc + d4*L_NABD_NrmStorWs_ratio +  ha5*W1_HAG + ph3*drought_mean
evap_index_sc ~ l5*LQLow_kmcl + b3*LQbkf_kmcl + p7*PSUMPY_SY_WS_sc + rn3*asin_PCTNATTERR_WsRp100 + ph5*drought_mean + sp2*L_STRM_POWER
LRBS_use ~ l2*LQLow_kmcl + b2*LQbkf_kmcl + ha2*W1_HAG + p2*PSUMPY_SY_WS_sc
L_NTL ~ u2*asin_PCTURB_WS + na2*W1_HNOAG + l3*LQLow_kmcl + p5*PSUMPY_SY_WS_sc
L_SULF ~ u4*asin_PCTURB_WS + e2*evap_index_sc + na3*W1_HNOAG + l4*LQLow_kmcl + p6*PSUMPY_SY_WS_sc + ph4*drought_mean

OE_SCORE ~ r1*LRBS_use + n1*L_NTL + su1*L_SULF + a1*asin_PCTAGR_WS + rn1*asin_PCTNATTERR_WsRp100 + w1*asin_PCTWET_WsRp100 + sp1*L_STRM_POWER

# INDIRECT EFFECTS ON OE
urb_oe:= u2*n1 + u3*b2*r1 + u3*b3*e2*su1 + u4*su1
agr_oe:= a2*l2*r1 + a2*l3*n1 + a2*l4*su1 + a2*l5*e2*su1
dam_oe:= d2*sp1 + d2*sp2*e2*su1 + d3*l2*r1 + d3*l3*n1 + d3*l4*su1 + d3*l5*e2*su1 + d4*b2*r1 + d4*b3*e2*su1
precip_oe:= p2*r1 + p3*l2*r1 + p3*l3*n1 + p3*l4*su1 + p3*l5*e2*su1 + p4*b2*r1 + p4*b3*e2*su1 + p5*n1 + p6*su1 + p7*e2*su1
phdi_oe:= ph2*sp1 + ph2*sp2*e2*su1 + ph3*b2*r1 + ph3*b3*e2*su1 + ph4*su1 + ph5*e2*su1
strpwr_oe:= sp2*e2*su1
hag_oe:= ha2*r1 + ha3*sp1 + ha3*sp2*e2*su1 + ha4*l2*r1 + ha4*l3*n1 + ha4*l4*su1 + ha4*l5*e2*su1 + ha5*b2*r1 + ha5*b3*e2*su1
nhag_oe:= na2*n1 + na3*su1 + na4*l2*r1 + na4*l3*n1 + na4*l4*su1 + na4*l5*e2*su1
rnat_oe:= rn2*sp1 + rn2*sp2*e2*su1 + rn3*e2*su1
rwet_oe:= w2*l2*r1 + w2*l3*n1 + w2*l4*su1 + w2*l5*e2*su1
lflow_oe:= l2*r1 + l3*n1 + l4*su1 + l5*e2*su1
bflow_oe:= b2*r1 + b3*e2*su1
dexcess_oe:= e2*su1

# TOTAL EFFECTS ON OE
urb_tot:= urb_oe
agr_tot:= a1 + agr_oe
dam_tot:= dam_oe
precip_tot:= precip_oe
phdi_tot:= phdi_oe
strpwr_tot:= sp1 + strpwr_oe
hag_tot:= hag_oe
nhag_tot:= nhag_oe
rnat_tot:= rn1 + rnat_oe
rwet_tot:= w1 + rwet_oe
lflow_tot:= lflow_oe
bflow_tot:= bflow_oe
dexcess_tot:= dexcess_oe

rbs_tot:= r1
tn_tot:= n1
sulf_tot:= su1

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_NTL ~~    L_SULF

'

# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_XERw_OE_rev_robust.est<- sem(mymodel_v15_OE_rev_xer, data=xer_w,
                                     estimator="MLM")

summary(fit_v15_XERw_OE_rev_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_OE_rev_robust.est)
print(mi_min[mi_min$mi >3.0,])


######################
## Bollen.stine bootstrap to estimate parameters -OE
fit_v15_XERw_OE_bootstrap_rev  <- sem(mymodel_v15_OE_rev_xer, data=xer_w,
                                      #group = "ECOREG_rev",
                                      #missing="ML",
                                      test="bollen.stine", se="boot",bootstrap=1000)

summary(fit_v15_XERw_OE_bootstrap_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_OE_bootstrap_rev)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - XERIC OE MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v15_XERw_OE_rev<- capture.output(summary(fit_v15_XERw_OE_bootstrap_rev, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v15_XERw_OE_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_OE_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v15_XERw_OE_bootstrap_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_OE_CI.csv",
          row.names = FALSE)

##################
# Save model fit parameters
## GRAB R2
r2_XER_OE<-parameterEstimates(fit_v15_XERw_OE_bootstrap_rev,rsquare=TRUE)%>%
  filter(op=="r2")

write.csv(r2_XER_OE,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_OE_R2.csv",
          row.names=FALSE)

# GRAB R2 to be able to add to fit table
r2_red_OE <- r2_XER_OE%>%
  filter(lhs=="OE_SCORE")%>%
  select(est)%>%
  mutate(across(where(is.numeric), round,2))

# TABLE OF FIT INDICES comparing 3 RESPONSES
table_fit_oe_xer <- matrix(NA, nrow=2, ncol=11)
colnames(table_fit_oe_xer) = c("Model","Estimation","X2", "df","CFI","TLI", "RMSEA","SRMR","AIC","n","npar")

table_fit_oe_xer[1,]<-c("OE","Standard",round(fitmeasures(fit_v15_XERw_OE_rev_robust.est,
                                                          c("chisq","df","cfi","tli",
                                                            "rmsea","srmr","aic","ntotal","npar")),2))
table_fit_oe_xer[2,]<-c("OE","Robust",round(fitmeasures(fit_v15_XERw_OE_rev_robust.est,
                                                        c("chisq.scaled","df.scaled","cfi.scaled","tli.scaled",
                                                          "rmsea.robust","srmr","aic","ntotal","npar")),2))
#table_fit_oe_xer

# Add R2 for biotic response
table_fit_oe_xer<- data.frame(table_fit_oe_xer)%>%
  mutate(R2 = r2_red_OE$est)

write.csv(table_fit_oe_xer, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_OE_fit.csv",
          row.names = FALSE)


###########################################
###########################################
## PATH MODEL v15 MMI
mymodel_v15_MMI <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_STRM_POWER + PSUMPY_SY_WS_sc + drought_mean
L_STRM_POWER ~PSUMPY_SY_WS_sc + drought_mean + L_NABD_NrmStorWs_ratio + W1_HAG + asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + drought_mean
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + drought_mean
evap_index_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + drought_mean + L_STRM_POWER
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl + LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_STRM_POWER + drought_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean

MMI_BENT_sc ~ Lpt01_XCMGW + LRBS_use + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_STRM_POWER + drought_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF
L_PTL ~~ L_TURB

'
##############
## XER MMI
# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_XERw_MMI_robust.est<- sem(mymodel_v15_MMI, data=xer_w,
                                  estimator="MLM")

summary(fit_v15_XERw_MMI_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_MMI_robust.est)
print(mi_min[mi_min$mi >3.0,])


#####################
## REVISED XER MMI
mymodel_v15_MMI_rev <- '
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

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_NTL ~~                  L_SULF

'
# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_XERw_MMI_rev_robust.est<- sem(mymodel_v15_MMI_rev, data=xer_w,
                                      estimator="MLM")

summary(fit_v15_XERw_MMI_rev_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_MMI_rev_robust.est)
print(mi_min[mi_min$mi >3.0,])


######################
## Bollen.stine bootstrap to estimate parameters
fit_v15_XERw_MMI_bootstrap_rev  <- sem(mymodel_v15_MMI_rev, data=xer_w,
                                       #group = "ECOREG_rev",
                                       #missing="ML",
                                       test="bollen.stine", se="boot",bootstrap=1000)

summary(fit_v15_XERw_MMI_bootstrap_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_MMI_bootstrap_rev)
print(mi_min[mi_min$mi >3.0,])


#############
# Export R output - MMI MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v15_XERw_MMI_rev<- capture.output(summary(fit_v15_XERw_MMI_bootstrap_rev, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v15_XERw_MMI_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_MMI_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v15_XERw_MMI_bootstrap_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_MMI_CI.csv",
          row.names = FALSE)

##################
# Save model fit parameters
## GRAB R2
r2_XER_MMI<-parameterEstimates(fit_v15_XERw_MMI_bootstrap_rev,rsquare=TRUE)%>%
  filter(op=="r2")

write.csv(r2_XER_MMI,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_MMI_R2.csv",
          row.names=FALSE)

# GRAB R2 to be able to add to fit table
r2_red_MMI <- r2_XER_MMI%>%
  filter(lhs=="MMI_BENT_sc")%>%
  select(est)%>%
  mutate(across(where(is.numeric), round,2))

# TABLE OF FIT INDICES comparing 3 RESPONSES
table_fit_MMI <- matrix(NA, nrow=2, ncol=11)
colnames(table_fit_MMI) = c("Model","Estimation","X2", "df","CFI","TLI", "RMSEA","SRMR","AIC","n","npar")

table_fit_MMI[1,]<-c("MMI","Standard",round(fitmeasures(fit_v15_XERw_MMI_rev_robust.est,
                                                        c("chisq","df","cfi","tli",
                                                          "rmsea","srmr","aic","ntotal","npar")),2))
table_fit_MMI[2,]<-c("MMI","Robust",round(fitmeasures(fit_v15_XERw_MMI_rev_robust.est,
                                                      c("chisq.scaled","df.scaled","cfi.scaled","tli.scaled",
                                                        "rmsea.robust","srmr","aic","ntotal","npar")),2))
# Add R2 for biotic response
table_fit_MMI<- data.frame(table_fit_MMI)%>%
  mutate(R2 = r2_red_MMI$est)

write.csv(table_fit_MMI, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_MMI_fit.csv",
          row.names = FALSE)


###########################################
###########################################
## PATH MODEL v15 EPT
mymodel_v15_EPT <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_STRM_POWER + PSUMPY_SY_WS_sc + drought_mean
L_STRM_POWER ~PSUMPY_SY_WS_sc + drought_mean + L_NABD_NrmStorWs_ratio + W1_HAG + asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + drought_mean
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + drought_mean
evap_index_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTNATTERR_WsRp100 + drought_mean + L_STRM_POWER
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl + LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_STRM_POWER + drought_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HAG + W1_HNOAG + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl + LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean

EPT_RICH_sc ~ Lpt01_XCMGW + LRBS_use + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTNATTERR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_STRM_POWER + drought_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF
L_PTL ~~ L_TURB

'
##############
## XER EPT
# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_XERw_EPT_robust.est<- sem(mymodel_v15_EPT, data=xer_w,
                                  estimator="MLM")

summary(fit_v15_XERw_EPT_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_EPT_robust.est)
print(mi_min[mi_min$mi >3.0,])


#####################
## REVISED XER EPT
mymodel_v15_EPT_rev <- '
Lpt01_XCMGW ~ a2*asin_PCTAGR_WS + u2*asin_PCTURB_WS + ha3*W1_HAG + na2*W1_HNOAG + sp2*L_STRM_POWER
L_STRM_POWER ~ ph2*drought_mean + d2*L_NABD_NrmStorWs_ratio + ha4*W1_HAG + rn2*asin_PCTNATTERR_WsRp100 # Modification index indicated better fit
LQLow_kmcl ~ a3*asin_PCTAGR_WS + p3*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 + ha5*W1_HAG + na4*W1_HNOAG
LQbkf_kmcl ~ u4*asin_PCTURB_WS + p4*PSUMPY_SY_WS_sc + d4*L_NABD_NrmStorWs_ratio +  ha6*W1_HAG + ph3*drought_mean
evap_index_sc ~ l5*LQLow_kmcl + b3*LQbkf_kmcl + p7*PSUMPY_SY_WS_sc + rn3*asin_PCTNATTERR_WsRp100 + ph5*drought_mean + sp3*L_STRM_POWER
LRBS_use ~ l2*LQLow_kmcl + b2*LQbkf_kmcl + ha2*W1_HAG + p2*PSUMPY_SY_WS_sc
L_NTL ~ u3*asin_PCTURB_WS + na3*W1_HNOAG + l3*LQLow_kmcl + p5*PSUMPY_SY_WS_sc
L_SULF ~ u5*asin_PCTURB_WS + e2*evap_index_sc + na5*W1_HNOAG + l4*LQLow_kmcl + p6*PSUMPY_SY_WS_sc + ph4*drought_mean

EPT_RICH_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + su1*L_SULF + rn1*asin_PCTNATTERR_WsRp100


# INDIRECT EFFECTS ON EPT
urb_ept:= u2*x1 + u3*n1 + u4*b2*r1 + u4*b3*e2*su1 + u5*su1
agr_ept:= a2*x1 + a3*l2*r1 + a3*l3*n1 + a3*l4*su1 + a3*l5*e2*su1
dam_ept:= d2*sp2*x1 + d2*sp3*e2*su1 + d3*l2*r1 + d3*l3*n1 + d3*l4*su1 + d3*l5*e2*su1 + d4*b2*r1 + d4*b3*e2*su1
precip_ept:= p2*r1 + p3*l2*r1 + p3*l3*n1 + p3*l4*su1 + p3*l5*e2*su1 + p4*b2*r1 + p4*b3*e2*su1 + p5*n1 + p6*su1 + p7*e2*su1
phdi_ept:= ph2*sp2*x1 + ph2*sp3*e2*su1 + ph3*b2*r1 + ph3*b3*e2*su1 + ph4*su1 + ph5*e2*su1
strpwr_ept:= sp2*x1 + sp3*e2*su1
hag_ept:= ha2*r1 + ha3*x1 + ha4*sp2*x1 + ha4*sp3*e2*su1 + ha5*l2*r1 + ha5*l3*n1 + ha5*l4*su1 + ha5*l5*e2*su1 + ha6*b2*r1 + ha6*b3*e2*su1
nhag_ept:= na2*x1 + na3*n1 + na4*l2*r1 + na4*l3*n1 + na4*l4*su1 + na4*l5*e2*su1 + na5*su1
rnat_ept:= rn2*sp2*x1 + rn2*sp3*e2*su1 + rn3*e2*su1
rwet_ept:= w2*l2*r1 + w2*l3*n1 + w2*l4*su1 + w2*l5*e2*su1
lflow_ept:= l2*r1 + l3*n1 + l4*su1 + l5*e2*su1
bflow_ept:= b2*r1 + b3*e2*su1
dexcess_ept:= e2*su1

# TOTAL EFFECTS ON EPT
urb_tot:= urb_ept
agr_tot:= agr_ept
dam_tot:= dam_ept
precip_tot:= precip_ept
phdi_tot:= phdi_ept
strpwr_tot:= strpwr_ept
hag_tot:= hag_ept
nhag_tot:= nhag_ept
rnat_tot:= rn1 + rnat_ept
rwet_tot:= rwet_ept
lflow_tot:= lflow_ept
bflow_tot:= bflow_ept
dexcess_tot:= dexcess_ept
xcmgw_tot:= x1
rbs_tot:= r1
tn_tot:= n1
sulf_tot:= su1

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_STRM_POWER~~LQLow_kmcl
L_STRM_POWER~~LQbkf_kmcl
L_NTL ~~                  L_SULF

'

# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v15_XERw_EPT_rev_robust.est<- sem(mymodel_v15_EPT_rev, data=xer_w,
                                      estimator="MLM")

summary(fit_v15_XERw_EPT_rev_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_EPT_rev_robust.est)
print(mi_min[mi_min$mi >3.0,])


######################
## Bollen.stine bootstrap to estimate parameters -OE
fit_v15_XERw_EPT_bootstrap_rev  <- sem(mymodel_v15_EPT_rev, data=xer_w,
                                       #group = "ECOREG_rev",
                                       #missing="ML",
                                       test="bollen.stine", se="boot",bootstrap=1000)

summary(fit_v15_XERw_EPT_bootstrap_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v15_XERw_EPT_bootstrap_rev)
print(mi_min[mi_min$mi >3.0,])


#############
# Export R output - EPT MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v15_XERw_EPT_rev<- capture.output(summary(fit_v15_XERw_EPT_bootstrap_rev, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v15_XERw_EPT_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_EPT_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v15_XERw_EPT_bootstrap_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_EPT_CI.csv",
          row.names = FALSE)

##################
# Save model fit parameters
## GRAB R2
r2_XER_EPT<-parameterEstimates(fit_v15_XERw_EPT_bootstrap_rev,rsquare=TRUE)%>%
  filter(op=="r2")

write.csv(r2_XER_EPT,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_EPT_R2.csv",
          row.names=FALSE)

# GRAB R2 to be able to add to fit table
r2_red_EPT <- r2_XER_EPT%>%
  filter(lhs=="EPT_RICH_sc")%>%
  select(est)%>%
  mutate(across(where(is.numeric), round,2))

# TABLE OF FIT INDICES comparing 3 RESPONSES
table_fit_EPT <- matrix(NA, nrow=2, ncol=11)
colnames(table_fit_EPT) = c("Model","Estimation","X2", "df","CFI","TLI", "RMSEA","SRMR","AIC","n","npar")

table_fit_EPT[1,]<-c("EPT","Standard",round(fitmeasures(fit_v15_XERw_EPT_rev_robust.est,
                                                        c("chisq","df","cfi","tli",
                                                          "rmsea","srmr","aic","ntotal","npar")),2))
table_fit_EPT[2,]<-c("EPT","Robust",round(fitmeasures(fit_v15_XERw_EPT_rev_robust.est,
                                                      c("chisq.scaled","df.scaled","cfi.scaled","tli.scaled",
                                                        "rmsea.robust","srmr","aic","ntotal","npar")),2))
# Add R2 for biotic response
table_fit_EPT<- data.frame(table_fit_EPT)%>%
  mutate(R2 = r2_red_EPT$est)

write.csv(table_fit_EPT, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/XERw_m15_EPT_fit.csv",
          row.names = FALSE)
