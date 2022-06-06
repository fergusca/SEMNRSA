############################
## PATH MODELS PREDICTING MMI AND EPT
##  MODELS DESIGNED TO INCLUDE NATURAL DRIVERS
## SOUTHERN PLAINS

## 6/2/2022
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

#############
# STREAM MORPH TRANSFORMATIONS
#dat_proc<-dat_proc%>%
#  mutate(L_XWXD = log10(XWXD+0.01),
#         L_SLOPE = log10(XSLOPE_use+0.01),
#         L_SLOPE_DPTH = log10((XSLOPE_use+0.01)*XDEPTH_CM))
#summary(dat_proc$L_SLOPE_DPTH)

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
#         CPL NAP NPL SAP SPL TPL UMW WMT XER
#BOATABLE 328 220  69 258  58 192 177 176 157
#WADEABLE 269 284 302 379 331 322 223 341 285


################
## SPL DATA SUBSET n = 363 w/216 vars
spl<-dat_proc%>%
  filter(AG_ECO9=="SPL")%>%
  drop_na(LOE_QLow_cl)

summary(spl$LOE_Qbkf_cl)

# SCALE CUMULATIVE PRECIPITATION in spl
spl$PSUMPY_SY_WS_sc<-scale(spl$PSUMPY_SY_WS)
summary(spl$PSUMPY_SY_WS_sc)

# WADEABLE n = 308
spl_w <- spl %>%
  filter(PROTOCOL=="WADEABLE")

# BOATABLE n = 55
spl_b <-spl %>%
  filter(PROTOCOL=="BOATABLE")

####################################
#########################
## PATH MODEL v9 - USING UNSCALED FIELD MEASURED PREDICTORS - NOT DEVIATION FROM REFERENCE
#  INCLUDING NATURAL FACTORS OMITTED IN THE OE DESIGNED MODEL
mymodel_v9 <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + phdi_mean + L_SLOPE_DPTH
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
d.excess_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + phdi_mean
Lpt01_XFC_NAT ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean

MMI_BENT_sc ~ Lpt01_XCMGW + LRBS_use + Lpt01_XFC_NAT + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF

'

fit_v9_SPL_wade <- sem(mymodel_v9 , data=spl_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap")

summary(fit_v9_SPL_wade, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_SPL_wade)
print(mi_min[mi_min$mi >3.0,])




#########################
## PATH MODEL v9 - REVISED - DROPPING ns relationships p>0.1
mymodel_v9_rev <- '
Lpt01_XCMGW ~ a3*asin_PCTAGR_WS + h2*W1_HAG + f2*asin_PCTFOR_WsRp100 + t3*Tmax8110Ws
LQLow_kmcl~ p3*PSUMPY_SY_WS_sc + d2*L_NABD_NrmStorWs_ratio + nh2*W1_HNOAG + t2*Tmax8110Ws + s3*L_SLOPE_DPTH
LQbkf_kmcl ~ p4*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + nh3*W1_HNOAG + s4*L_SLOPE_DPTH + ph3*phdi_mean
LRBS_use ~ a2*asin_PCTAGR_WS + l2*LQLow_kmcl+ b2*LQbkf_kmcl + p2*PSUMPY_SY_WS_sc + s2*L_SLOPE_DPTH + ph2*phdi_mean
L_PTL ~ a4*asin_PCTAGR_WS +  h3*W1_HAG + w2*asin_PCTWET_WsRp100 + p5*PSUMPY_SY_WS_sc + ph4*phdi_mean

MMI_BENT_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + tp1*L_PTL + s1*L_SLOPE_DPTH + t1*Tmax8110Ws

# Covariance
LQLow_kmcl~~LQbkf_kmcl

# INDIRECT MMI EFFECTS
agr_m:= a2*r1 + a3*x1 + a4*tp1
dam_m:= d2*l2*r1 + d3*b2*r1
tmax_m:= t2*l2*r1 + t3*x1
precip_m:= p2*r1 + p3*l2*r1 + p4*b2*r1 + p5*tp1
phdi_m:= ph2*r1 + ph3*b2*r1 + ph4*tp1
slope_m:= s2*r1 + s3*l2*r1 + s4*b2*r1
nohag_m:= nh2*l2*r1 + nh3*b2*r1
hag_m:= h2*x1 + h3*tp1
rfor_m:= f2*x1
rwet_m:= w2*tp1
lflow_m:= l2*r1
bflow_m:= b2*r1

# TOTAL MMI EFFECTS
tot_agr:= agr_m
tot_dam:= dam_m
tot_tmax:= t1 + tmax_m
tot_precip:= precip_m
tot_phdi:= phdi_m
tot_slope:= s1 + slope_m
tot_nohag:= nohag_m
tot_hag:= hag_m
tot_rfor:= rfor_m
tot_rwet:= rwet_m
tot_lflow:= lflow_m
tot_bflow:= bflow_m
tot_xcmgw:= x1
tot_rbs:= r1
tot_tp:= tp1

'

fit_v9_SPL_wade_rev <- sem(mymodel_v9_rev , data=spl_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap")

summary(fit_v9_SPL_wade_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_SPL_wade_rev)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_SPL_wade_rev<- capture.output(summary(fit_v9_SPL_wade_rev, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_SPL_wade_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/SPLw_m9_MMI_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_SPL_wade_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/SPLw_m9_MMI_rev_CI.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9<- lavaanPlot(model=fit_v9_SPL_wade_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_MMI_rev_SPLw.png")



#################################
################################
## EPT
#########################
## PATH MODEL v9 - EPT USING UNSCALED FIELD MEASURED PREDICTORS - NOT DEVIATION FROM REFERENCE
#  INCLUDING NATURAL FACTORS OMITTED IN THE OE DESIGNED MODEL
mymodel_v9_ept <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + phdi_mean + L_SLOPE_DPTH
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
d.excess_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + phdi_mean
Lpt01_XFC_NAT ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean

EPT_RICH_sc ~ Lpt01_XCMGW + LRBS_use + Lpt01_XFC_NAT + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF


'

fit_v9_SPL_wade_ept <- sem(mymodel_v9_ept , data=spl_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap")

summary(fit_v9_SPL_wade_ept, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_SPL_wade_ept)
print(mi_min[mi_min$mi >3.0,])

###############
## REVISED
mymodel_v9_ept_rev <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + W1_HAG + asin_PCTFOR_WsRp100 + Tmax8110Ws
LQLow_kmcl~ PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio+ W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH
LQbkf_kmcl ~ PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + W1_HNOAG + L_SLOPE_DPTH + phdi_mean
d.excess_sc ~  PSUMPY_SY_WS_sc + Tmax8110Ws
LRBS_use ~ asin_PCTAGR_WS + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + phdi_mean
L_PTL ~ asin_PCTAGR_WS +  W1_HAG + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + phdi_mean
L_SULF ~ d.excess_sc + W1_HNOAG + asin_PCTWET_WsRp100 + LQLow_kmcl + L_NABD_NrmStorWs_ratio
L_TURB ~ asin_PCTAGR_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio

EPT_RICH_sc ~ Lpt01_XCMGW + LRBS_use + L_PTL + L_SULF + L_TURB + L_SLOPE_DPTH

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~                 L_TURB

# INDIRECT EPT EFFECTS


'

fit_v9_SPL_wade_ept_rev <- sem(mymodel_v9_ept_rev , data=spl_w,
                           #group = "ECOREG_rev",
                           missing="ML", se="bootstrap")

summary(fit_v9_SPL_wade_ept_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_SPL_wade_ept_rev)
print(mi_min[mi_min$mi >3.0,])


#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_SPL_wade_ept_rev<- capture.output(summary(fit_v9_SPL_wade_ept_rev, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_SPL_wade_ept_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/SPLw_m9_EPT_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_SPL_wade_ept_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/SPLw_m9_EPT_rev_CI.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9_ept<- lavaanPlot(model=fit_v9_SPL_wade_ept_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9_ept,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_EPT_rev_SPLw.png")


#################################
################################
## BENTHIC O/E
#########################
## PATH MODEL v9 - EPT USING UNSCALED FIELD MEASURED PREDICTORS - NOT DEVIATION FROM REFERENCE
#  INCLUDING NATURAL FACTORS OMITTED IN THE OE DESIGNED MODEL
mymodel_v9_oe <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + phdi_mean + L_SLOPE_DPTH
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
d.excess_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + phdi_mean
Lpt01_XFC_NAT ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean

OE_SCORE ~ Lpt01_XCMGW + LRBS_use + Lpt01_XFC_NAT + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_SLOPE_DPTH + phdi_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF


'

fit_v9_SPL_wade_oe <- sem(mymodel_v9_oe , data=spl_w,
                           #group = "ECOREG_rev",
                           missing="ML", se="bootstrap")

summary(fit_v9_SPL_wade_oe, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_SPL_wade_oe)
print(mi_min[mi_min$mi >3.0,])

###############
## REVISED
mymodel_v9_oe_rev <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + W1_HAG + asin_PCTFOR_WsRp100 + Tmax8110Ws
LQLow_kmcl~ PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH
LQbkf_kmcl ~ PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + W1_HNOAG + L_SLOPE_DPTH + phdi_mean
#d.excess_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
Lpt01_XFC_NAT ~ Lpt01_XCMGW  + LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws
L_TURB ~ asin_PCTAGR_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio

OE_SCORE ~ Lpt01_XCMGW + Lpt01_XFC_NAT + L_TURB + LQLow_kmcl

# Covariance
LQLow_kmcl~~LQbkf_kmcl

# INDIRECT OE EFFECTS


'

fit_v9_SPL_wade_oe_rev <- sem(mymodel_v9_oe_rev , data=spl_w,
                          #group = "ECOREG_rev",
                          missing="ML", se="bootstrap")

summary(fit_v9_SPL_wade_oe_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_SPL_wade_oe_rev)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_SPL_wade_oe_rev<- capture.output(summary(fit_v9_SPL_wade_oe_rev, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_SPL_wade_oe_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/SPLw_m9_OE_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_SPL_wade_oe_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/SPLw_m9_OE_rev_CI.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9_oe<- lavaanPlot(model=fit_v9_SPL_wade_oe_rev, node_options=list(shape="box",fontname= "Helvetica"),
                    edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9_oe,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_OE_rev_SPLw.png")

