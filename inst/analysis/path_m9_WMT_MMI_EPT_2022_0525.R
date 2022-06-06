############################
## PATH MODELS PREDICTING MMI AND EPT
##  MODELS DESIGNED TO INCLUDE NATURAL DRIVERS
##
## 5/25/2022
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

###################
## QUICK CORRELATION PLOTS
#####################
## CORRELATION
# Stream morph
morph<-dat_proc %>%
  select(c(L_SLOPE,L_SLOPE_DPTH,
           L_XWIDTH_use,
           MMI_BENT_sc,
           EPT_RICH_sc,OE_SCORE))%>%
  #LOE_XCMGW_use,LOE_XFC_NAT_use, LOE_RBS_use)) %>%
  GGally::ggpairs()
morph

subst<- dat_proc%>%
  select(c(LSUB_DMM,LDCBF_use,LRBS_use,
           MMI_BENT_sc,
           EPT_RICH_sc,OE_SCORE))%>%
  #LOE_XCMGW_use,LOE_XFC_NAT_use, LOE_RBS_use)) %>%
  GGally::ggpairs()
subst

# Climate
clim<-dat_proc %>%
  select(c(Tmax8110Ws, TMEAN_SY_WS,
           Precip8110Ws,PSUMPY_SY_WS,
           phdi_month,phdi_mean,
           MMI_BENT_sc,
           EPT_RICH_sc,OE_SCORE))%>%
  na.omit()%>%
  #LOE_XCMGW_use,LOE_XFC_NAT_use, LOE_RBS_use)) %>%
  GGally::ggpairs()
clim


################
## WMT DATA SUBSET n = 490 w/205 vars
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

fit_v9_WMT_wade <- sem(mymodel_v9 , data=wmt_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap")

summary(fit_v9_WMT_wade, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WMT_wade)
print(mi_min[mi_min$mi >3.0,])




#########################
## PATH MODEL v9 - REVISED - DROPPING ns relationships p>0.1
mymodel_v9_rev <- '
Lpt01_XCMGW ~ u3*asin_PCTURB_WS + h3*W1_HAG + f2*asin_PCTFOR_WsRp100 + s3*L_SLOPE_DPTH
LQLow_kmcl~ u4*asin_PCTURB_WS + p4*PSUMPY_SY_WS_sc + h4*W1_HAG + t2*Tmax8110Ws + s4*L_SLOPE_DPTH
LQbkf_kmcl ~ p3*PSUMPY_SY_WS_sc + d2*L_NABD_NrmStorWs_ratio + h5*W1_HAG + s5*L_SLOPE_DPTH + t3*Tmax8110Ws + pi3*phdi_mean
d.excess_sc ~  l3*LQLow_kmcl+ b3*LQbkf_kmcl + p5*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + f3*asin_PCTFOR_WsRp100 + pi4*phdi_mean
LRBS_use ~ u2*asin_PCTURB_WS + l2*LQLow_kmcl + b2*LQbkf_kmcl + h2*W1_HAG + p2*PSUMPY_SY_WS_sc + s2*L_SLOPE_DPTH + pi2*phdi_mean
L_NTL ~ u5*asin_PCTURB_WS + x2*Lpt01_XCMGW + e2*d.excess_sc + w2*asin_PCTWET_WsRp100

MMI_BENT_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + f1*asin_PCTFOR_WsRp100 + s1*L_SLOPE_DPTH

# Covariance
LQLow_kmcl~~LQbkf_kmcl

# INDIRECT MMI EFFECTS
urb_m:=u2*r1+u3*x1+u4*l2*r1 + u4*l3*e2*n1 + u5*n1
tmax_m:= t2*l2*r1 + t2*l3*e2*n1 + t3*b2*r1 + t3*b3*e2*n1
precip_m:= p2*r1 + p3*b2*r1 + p3*b3*e2*n1 +p4*l2*r1 + p4*l3*e2*n1 + p3*e2*n1
phdi_m:= pi2*r1 + pi3*b2*r1 + pi3*b2*e2*n1 + pi4*e2*n1
dam_m:= d2*b2*r1 + d2*b3*e2*n1 + d3*e2*n1
slope_m:= s2*r1 + s3*x1 + s4*l2*r1 + s4*l3*e2*n1 + s5*b2*r1 + s5*b3*e2*n1
hag_m:= h2*r1 + h3*x1 + h4*l2*r1 + h4*l3*e2*n1 + h5*b2*r1 + h5*b3*e2*n1
lflow_m:= l2*r1 + l3*e2*n1
bflow_m:= b2*r1 + b3*e2*n1
dexcess_m:= e2*n1
rfor_m:= f2*x1 + f3*e2*n1
rwet_m:= w2*n1
xcmgw_m:= x2*n1

# TOTAL EFFECTS on MMI
tot_urb:= urb_m
tot_tmax:= tmax_m
tot_precip:= precip_m
tot_phdi:= phdi_m
tot_dam:= dam_m
tot_slope:= s1 + slope_m
tot_hag:= hag_m
tot_lflow:=lflow_m
tot_bflow:= bflow_m
tot_dexcess:= dexcess_m
tot_rfor:= f1 + rfor_m
tot_rwet:= rwet_m
tot_xcmgw:= x1 + xcmgw_m
tot_rbs:= r1
tot_ntl:= n1

'

fit_v9_WMT_wade_rev <- sem(mymodel_v9_rev , data=wmt_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap")

summary(fit_v9_WMT_wade_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WMT_wade_rev)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_WMT_wade_rev<- capture.output(summary(fit_v9_WMT_wade_rev, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_WMT_wade_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9_MMI_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_WMT_wade_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9_MMI_rev_CI.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9<- lavaanPlot(model=fit_v9_WMT_wade_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_MMI_rev_WMTw.png")

#############################
## MMI WITH DIFFERENT SUBSTRATE PREDICTORS
#########################
## PATH MODEL v9b  - USING UNSCALED FIELD MEASURED PREDICTORS - NOT DEVIATION FROM REFERENCE
#  INCLUDING NATURAL FACTORS OMITTED IN THE OE DESIGNED MODEL
mymodel_v9b <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + phdi_mean
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
d.excess_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
PCT_SAFN ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + phdi_mean
LDCBF_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + phdi_mean
#LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + phdi_mean
Lpt01_XFC_NAT ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + phdi_mean

MMI_BENT_sc ~ Lpt01_XCMGW + PCT_SAFN + LDCBF_use + Lpt01_XFC_NAT + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_SLOPE_DPTH + Tmax8110Ws + phdi_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF

'

fit_v9b_WMT_wade <- sem(mymodel_v9b , data=wmt_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap")

summary(fit_v9b_WMT_wade, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9b_WMT_wade)
print(mi_min[mi_min$mi >3.0,])

###############
## REVISED V9b MMI model w/ different substrate predictors
mymodel_v9b_rev <- '
Lpt01_XCMGW ~ asin_PCTURB_WS + W1_HAG + asin_PCTFOR_WsRp100 + L_SLOPE_DPTH
LQLow_kmcl~ PSUMPY_SY_WS_sc + W1_HAG + Tmax8110Ws + L_SLOPE_DPTH
LQbkf_kmcl ~ PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + Tmax8110Ws + L_SLOPE_DPTH + phdi_mean
d.excess_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100
PCT_SAFN ~ asin_PCTURB_WS + W1_HAG + asin_PCTFOR_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH
L_NTL ~ asin_PCTURB_WS + Lpt01_XCMGW + d.excess_sc + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100

MMI_BENT_sc ~ Lpt01_XCMGW + PCT_SAFN + L_NTL + asin_PCTFOR_WsRp100

# Covariance
LQLow_kmcl~~LQbkf_kmcl

'

fit_v9b_WMT_wade_rev <- sem(mymodel_v9b_rev , data=wmt_w,
                        #group = "ECOREG_rev",
                        missing="ML", se="bootstrap")

summary(fit_v9b_WMT_wade_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9b_WMT_wade_rev)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9b_WMT_wade_rev<- capture.output(summary(fit_v9b_WMT_wade_rev, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9b_WMT_wade_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9b_MMI_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9b_WMT_wade_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9b_MMI_rev_CI.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9b<- lavaanPlot(model=fit_v9b_WMT_wade_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9b,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9b_MMI_rev_WMTw.png")




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

fit_v9_WMT_wade_ept <- sem(mymodel_v9_ept , data=wmt_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap")

summary(fit_v9_WMT_wade_ept, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WMT_wade_ept)
print(mi_min[mi_min$mi >3.0,])

###############
## REVISED
mymodel_v9_ept_rev <- '
Lpt01_XCMGW ~ u3*asin_PCTURB_WS + h3*W1_HAG + f3*asin_PCTFOR_WsRp100 + s3*L_SLOPE_DPTH
LQLow_kmcl ~ u5*asin_PCTURB_WS + a3*asin_PCTAGR_WS + p3*PSUMPY_SY_WS_sc + h4*W1_HAG + t2*Tmax8110Ws + s4*L_SLOPE_DPTH
LQbkf_kmcl ~ p4*PSUMPY_SY_WS_sc + d2*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 +  h5*W1_HAG + t3*Tmax8110Ws + s5*L_SLOPE_DPTH + pi3*phdi_mean
d.excess_sc ~  l3*LQLow_kmcl+ b4*LQbkf_kmcl + p5*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + f4*asin_PCTFOR_WsRp100
LRBS_use ~ u2*asin_PCTURB_WS + l2*LQLow_kmcl+ b2*LQbkf_kmcl + h2*W1_HAG + p2*PSUMPY_SY_WS_sc + s2*L_SLOPE_DPTH + pi2*phdi_mean
L_NTL ~ u4*asin_PCTURB_WS + x2*Lpt01_XCMGW + e2*d.excess_sc + w3*asin_PCTWET_WsRp100
L_SULF ~ a2*asin_PCTAGR_WS + f2*asin_PCTFOR_WsRp100 + w4*asin_PCTWET_WsRp100 + b3*LQbkf_kmcl + p6*PSUMPY_SY_WS_sc

EPT_RICH_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + su1*L_SULF + f1*asin_PCTFOR_WsRp100 + s1*L_SLOPE_DPTH + t1*Tmax8110Ws

# Covariance
LQLow_kmcl~~LQbkf_kmcl
Lpt01_XCMGW ~~  L_NTL

# INDIRECT EPT EFFECTS
urb_e:= u2*r1 + u3*x1 + u4*n1 + u5*l2*r1 + u5*l3*e2*n1
agr_e:= a2*su1 + a3*l2*r1 + a3*l3*e2*n1
dam_e:= d2*b2*r1 + d2*b3*su1 + d2*b4*e2*n1 + d3*e2*n1
tmax_e:= t2*l2*r1 + t2*l3*e2*n1 + t3*b2*r1 + t3*b3*su1 + t3*b4*e2*n1
precip_e:= p2*r1 + p3*l2*r1 + p3*l3*e2*n1 + p4*b2*r1 + p4*b3*su1 + p4*b4*e2*n1 + p5*e2*n1 + p6*su1
phdi_e:= pi2*r1 + pi3*b2*r1 + pi3*b3*su1 + pi3*b4*e2*n1
slope_e:= s2*r1 + s3*x1 + s4*l2*r1 + s4*l3*e2*n1 + s5*b2*r1 + s5*b3*su1 + s5*b4*e2*n1
hag_e:= h2*r1 + h3*x1 + h4*l2*r1 + h4*l3*e2*n1 + h5*b2*r1 + h5*b3*su1 + h5*b4*e2*n1
rfor_e:= f2*su1 + f3*x1 + f4*e2*n1
rwet_e:= w2*b2*r1 + w2*b3*su1 + w2*b4*e2*n1 + w3*n1 + w4*su1
lflow_e:= l2*r1 + l3*e2*n1
bflow_e:= b2*r1 + b3*su1 + b4*e2*n1
dexcess_e:= e2*n1
xcmgw_e:= x2*n1

#TOTAL EPT EFFECTS
tot_urb:= urb_e
tot_agr:= agr_e
tot_dam:= dam_e
tot_tmax:= t1 + tmax_e
tot_precip:= precip_e
tot_phdi:= phdi_e
tot_slope:= s1 + slope_e
tot_hag:= hag_e
tot_rfor:= f1 + rfor_e
tot_rwet:= rwet_e
tot_lflow:= lflow_e
tot_bflow:= bflow_e
tot_dexcess:= dexcess_e
tot_xcmgw:= x1 + xcmgw_e
tot_rbs:= r1
tot_ntl:= n1
tot_sulf:= su1

'

fit_v9_WMT_wade_ept_rev <- sem(mymodel_v9_ept_rev , data=wmt_w,
                           #group = "ECOREG_rev",
                           missing="ML", se="bootstrap")

summary(fit_v9_WMT_wade_ept_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WMT_wade_ept_rev)
print(mi_min[mi_min$mi >3.0,])


#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_WMT_wade_ept_rev<- capture.output(summary(fit_v9_WMT_wade_ept_rev, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_WMT_wade_ept_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9_EPT_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_WMT_wade_ept_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9_EPT_rev_CI.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9_ept<- lavaanPlot(model=fit_v9_WMT_wade_ept_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9_ept,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_EPT_rev_WMTw.png")


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

fit_v9_WMT_wade_oe <- sem(mymodel_v9_oe , data=wmt_w,
                           #group = "ECOREG_rev",
                           missing="ML", se="bootstrap")

summary(fit_v9_WMT_wade_oe, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WMT_wade_oe)
print(mi_min[mi_min$mi >3.0,])

###############
## REVISED
mymodel_v9_oe_rev <- '
Lpt01_XCMGW ~ u3*asin_PCTURB_WS + h3*W1_HAG + f2*asin_PCTFOR_WsRp100 + s4*L_SLOPE_DPTH
LQLow_kmcl~ u4*asin_PCTURB_WS + p3*PSUMPY_SY_WS_sc + h4*W1_HAG + t2*Tmax8110Ws + s5*L_SLOPE_DPTH
LQbkf_kmcl ~ p4*PSUMPY_SY_WS_sc + d2*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 +  h5*W1_HAG + t4*Tmax8110Ws + s6*L_SLOPE_DPTH + pi3*phdi_mean
d.excess_sc ~  l3*LQLow_kmcl+ b3*LQbkf_kmcl + p5*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + f3*asin_PCTFOR_WsRp100
LRBS_use ~ u2*asin_PCTURB_WS + l2*LQLow_kmcl+ b2*LQbkf_kmcl + h2*W1_HAG + p2*PSUMPY_SY_WS_sc + s2*L_SLOPE_DPTH + pi2*phdi_mean
Lpt01_XFC_NAT ~ x2*Lpt01_XCMGW + s3*L_SLOPE_DPTH + t3*Tmax8110Ws
L_NTL ~ u5*asin_PCTURB_WS + x3*Lpt01_XCMGW + e2*d.excess_sc + w3*asin_PCTWET_WsRp100

OE_SCORE ~ x1*Lpt01_XCMGW + r1*LRBS_use + fn1*Lpt01_XFC_NAT + n1*L_NTL + l1*LQLow_kmcl + s1*L_SLOPE_DPTH

# Covariance
LQLow_kmcl~~LQbkf_kmcl

# INDIRECT OE EFFECTS
urb_oe:=u2*r1 +u3*x1 + u3*x2*fn1 + u3*x3*n1 + u4*l1 + u4*l2*r1 + u4*l3*e2*n1 + u5*n1
dam_oe:= d2*b2*r1 + d2*b3*e2*n1 + d3*e2*n1
tmax_oe:= t2*l1 + t2*l2*r1 + t2*l3*e2*n1 + t3*fn1 + t4*b2*r1 + t4*b3*e2*n1
precip_oe:= p2*r1 + p3*l1 + p3*l2*r1 + p3*l3*e2*n1 + p4*b2*r1 + p4*b3*e2*n1 + p5*e2*n1
phdi_oe:= pi2*r1 + pi3*b2*r1 + pi3*b3*e2*n1
slope_oe:= s2*r1 + s3*fn1 + s4*x1 + s4*x2*fn1 + s4*x3*n1 + s5*l1 + s5*l2*r1 + s5*l3*e2*n1 + s6*b2*r1 + s6*b3*e2*n1
hag_oe:= h2*r1 + h3*x1 + h3*x2*fn1 + h3*x3*n1 + h4*l1 + h4*l2*r1 + h4*l3*e2*n1 + h5*b2*r1 + h5*b3*e2*n1
rfor_oe:= f2*x1 + f2*x2*fn1 + f2*x3*n1 + f3*e2*n1
rwet_oe:= w2*b2*r1 + w2*b3*e2*n1 + w3*n1
lflow_oe:= l2*r1 + l3*e2*n1
bflow_oe:= b2*r1 + b3*e2*n1
dexcess_oe:= e2*n1
xcmgw_oe:= x2*fn1 + x3*n1

# TOTAL OE EFFECTS
tot_urb:=urb_oe
tot_dam:= dam_oe
tot_tmax:= tmax_oe
tot_precip:= precip_oe
tot_phdi:= phdi_oe
tot_slope:= s1 + slope_oe
tot_hag:= hag_oe
tot_rfor:= rfor_oe
tot_rwet:= rwet_oe
tot_lflow:= l1 + lflow_oe
tot_bflow:= bflow_oe
tot_dexcess:= dexcess_oe
tot_xcmgw:= x1 + xcmgw_oe
tot_rbs:= r1
tot_fnat:= fn1
tot_ntl: = n1

'

fit_v9_WMT_wade_oe_rev <- sem(mymodel_v9_oe_rev , data=wmt_w,
                          #group = "ECOREG_rev",
                          missing="ML", se="bootstrap")

summary(fit_v9_WMT_wade_oe_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WMT_wade_oe_rev)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_WMT_wade_oe_rev<- capture.output(summary(fit_v9_WMT_wade_oe_rev, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_WMT_wade_oe_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9_OE_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_WMT_wade_oe_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m9_OE_rev_CI.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9_oe<- lavaanPlot(model=fit_v9_WMT_wade_oe_rev, node_options=list(shape="box",fontname= "Helvetica"),
                    edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9_oe,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_OE_rev_WMTw.png")

