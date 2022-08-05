############################
## PATH MODELS PREDICTING OE, MMI, and EPT
##  MODELS DESIGNED TO INCLUDE NATURAL DRIVERS
##    WEST ECO3
##
## 8/5/2022 - met with Travis Schmidt and revised model syntax to use robust estimation to fit model and bollen-stine test and bootstrap to get SE
## 7/29/2022 - revised d-excess to be evaporation index and phdi to be drought index
## 7/18/2022
## 6/03/2022
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

#########################
## SUBSET BY AGGREGATED 3- ECOREGION
# WEST n = 905
west<-dat_proc%>%
  filter(AG_ECO3=="WMTNS")%>%
  drop_na(LOE_QLow_cl)

summary(west$LOE_Qbkf_cl)

# SCALE CUMULATIVE PRECIPITATION in wmt
west$PSUMPY_SY_WS_sc<-scale(west$PSUMPY_SY_WS)
summary(west$PSUMPY_SY_WS_sc)

# WADEABLE n = 595
west_w <- west %>%
  filter(PROTOCOL=="WADEABLE")

# BOATABLE n = 310
west_b <-west %>%
  filter(PROTOCOL=="BOATABLE")

###################
## DISTRIBUTIONS
hist(west_w$MMI_BENT)
hist(west_w$MMI_BENT_sc)

####################################
#########################
## PATH MODEL v9 O/E - 1) FULL MODEL - USING UNSCALED FIELD MEASURED PREDICTORS - NOT DEVIATION FROM REFERENCE
#  INCLUDING NATURAL FACTORS OMITTED IN THE OE DESIGNED MODEL
mymodel_v9 <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws + drought_mean
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + drought_mean + L_SLOPE_DPTH
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH + drought_mean
evap_index_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + drought_mean
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + drought_mean
Lpt01_XFC_NAT ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + L_SLOPE_DPTH + Tmax8110Ws + drought_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean

OE_SCORE ~ Lpt01_XCMGW + LRBS_use + Lpt01_XFC_NAT + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_SLOPE_DPTH + Tmax8110Ws + drought_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF

'

# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v9_WEST_wade_robust.est<- sem(mymodel_v9, data=west_w,
                              estimator="MLM")

# Bootstrap for se but takes awhile - better to fit using faster approach and then get se
#fit_v9_WEST_wade <- sem(mymodel_v9 , data=west_w,
#                        #group = "ECOREG_rev",
#                        missing="ML", se="bootstrap")

summary(fit_v9_WEST_wade_robust.est, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_wade_robust.est)
print(mi_min[mi_min$mi >3.0,])


#########################
## PATH MODEL v9 - REVISED - DROPPING ns relationships p>0.05
mymodel_v9_rev <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + L_SLOPE_DPTH
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH
LQbkf_kmcl ~ asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + W1_HAG + L_SLOPE_DPTH + drought_mean
evap_index_sc ~  LQLow_kmcl+ LQbkf_kmcl + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100
LRBS_use ~ asin_PCTURB_WS + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + asin_PCTFOR_WsRp100 + PSUMPY_SY_WS_sc
L_NTL ~ asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc + W1_HNOAG + asin_PCTFOR_WsRp100 + LQLow_kmcl+ PSUMPY_SY_WS_sc
#L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc

OE_SCORE ~ LRBS_use + L_NTL + L_SLOPE_DPTH

# Covariance
LQLow_kmcl~~LQbkf_kmcl
#L_NTL ~~                 L_SULF

'

# ROBUST ESTIMATION MAX LIKELIHOOD METHOD
fit_v9_WEST_wade_robust.est_rev<- sem(mymodel_v9_rev, data=west_w,
                                  estimator="MLM")

summary(fit_v9_WEST_wade_robust.est_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_wade_robust.est_rev)
print(mi_min[mi_min$mi >3.0,])


######################
## Bollen.stine bootstrap to estimate parameters
fit_v9_WEST_wade_bootstrap_rev  <- sem(mymodel_v9_rev, data=west_w,
                                #group = "ECOREG_rev",
                                #missing="ML",
                                test="bollen.stine", se="boot",bootstrap=1000)

summary(fit_v9_WEST_wade_bootstrap_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_wade_bootstrap_rev)
print(mi_min[mi_min$mi >3.0,])



# PREVIOUS MODEL
mymodel_v9_rev <- '
Lpt01_XCMGW ~ a3*asin_PCTAGR_WS + u4*asin_PCTURB_WS + ha3*W1_HAG + na2*W1_HNOAG + f4*asin_PCTFOR_WsRp100 + s3*L_SLOPE_DPTH
LQLow_kmcl~ a4*asin_PCTAGR_WS + u7*asin_PCTURB_WS + p4*PSUMPY_SY_WS_sc + d2*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 + ha4*W1_HAG + na3*W1_HNOAG + t2*Tmax8110Ws + s2*L_SLOPE_DPTH
LQbkf_kmcl ~  u6*asin_PCTURB_WS + p6*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + ha5*W1_HAG + t3*Tmax8110Ws + s4*L_SLOPE_DPTH + ph2*drought_mean
evap_index_sc ~  l5*LQLow_kmcl+ b4*LQbkf_kmcl + d4*L_NABD_NrmStorWs_ratio + f6*asin_PCTFOR_WsRp100
LRBS_use ~ u3*asin_PCTURB_WS + l3*LQLow_kmcl+ b3*LQbkf_kmcl + ha2*W1_HAG + f3*asin_PCTFOR_WsRp100 + p3*PSUMPY_SY_WS_sc
L_NTL ~ u5*asin_PCTURB_WS + x2*Lpt01_XCMGW + e2*evap_index_sc +  na4*W1_HNOAG + f5*asin_PCTFOR_WsRp100 + w3*asin_PCTWET_WsRp100 + l4*LQLow_kmcl+  p5*PSUMPY_SY_WS_sc
L_SULF ~ a2*asin_PCTAGR_WS + u2*asin_PCTURB_WS + f2*asin_PCTFOR_WsRp100 + l2*LQLow_kmcl+ b2*LQbkf_kmcl + p2*PSUMPY_SY_WS_sc

OE_SCORE ~  r1*LRBS_use + n1*L_NTL + su1*L_SULF + a1*asin_PCTAGR_WS + u1*asin_PCTURB_WS + sd1*L_SLOPE_DPTH

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_NTL ~~L_SULF

# INDIRECT EFFECTS ON OE
urb_oe:= u2*su1 + u3*r1 + u4*x2*n1 + u5*n1 + u6*b2*su1 + u6*b3*r1 + u6*b4*e2*n1 + u7*l2*su1 + u7*l3*r1 + u7*l4*n1 + u7*l5*e2*n1
agr_oe:= a2*su1 + a3*x2*n1 + a4*l2*su1 + a4*l3*r1 + a4*l4*n1 + a4*l5*e2*n1
dam_oe:= d2*l2*su1 + d2*l3*r1 + d2*l4*n1 + d2*l5*e2*n1 + d3*b2*su1 + d3*b3*r1 + d3*b4*e2*n1 + d4*e2*n1
tmax_oe:= t2*l2*su1 + t2*l3*r1 + t2*l4*n1 + t2*l5*e2*n1 + t3*b2*su1 + t3*b3*r1 + t3*b4*e2*n1
precip_oe:= p2*su1 + p3*r1 + p4*l2*su1 + p4*l3*r1 + p4*l4*n1 + p4*l5*e2*n1 + p5*n1 + p6*b2*su1 + p6*b3*r1 + p6*b4*e2*n1
phdi_oe:= ph2*b2*su1 + ph2*b3*r1 + ph2*b4*e2*n1
slope_oe:= s2*l2*su1 + s2*l3*r1 + s2*l4*n1 + s2*l5*e2*n1 + s3*x2*n1 + s4*b2*su1 + s4*b3*r1 + s4*b4*e2*n1
hag_oe:= ha2*r1 + ha3*x2 + ha4*l2*su1 + ha4*l3*r1 + ha4*l4*n1 + ha5*b2*su1 + ha5*b3*r1 + ha5*b4*e2*n1
nhag_oe:= na2*x2 + na3*l2*su1 + na3*l3*r1 + na3*l4*n1 + na3*l5*e2*n1 + na4*b2*su1 + na4*b3*r1 + na4*b4*e2*n1
rfor_oe:= f2*su1 + f3*r1 + f4*x2*n1 + f5*n1 + f6*e2*n1
rwet_oe:= w2*l2*su1 + w2*l3*r1 + w2*l4*n1 + w2*l5*e2*n1
lflow_oe:= l2*su1 + l3*r1 + l4*n1 + l5*e2*n1
bflow_oe:= b2*su1 + b3*r1 + b4*e2*n1
dexcess_oe:= e2*n1
xcmgw_oe:= x2*n1

# TOTAL EFFECTS ON OE
urb_tot:= u1 + urb_oe
agr_tot:= a1 + agr_oe
dam_tot:= dam_oe
tmax_tot:= tmax_oe
precip_tot:= precip_oe
phdi_tot:= phdi_oe
slope_tot:= sd1 + slope_oe
hag_tot:= hag_oe
nhag_tot:= nhag_oe
rfor_tot:= rfor_oe
rwet_tot:= rwet_oe
lflow_tot:= lflow_oe
bflow_tot:= bflow_oe
dexcess_tot:= dexcess_oe
xcmgw_tot:= xcmgw_oe
rbs_tot:= r1
tn_tot:= n1
sulf_tot:= su1

'

fit_v9_WEST_wade_rev <- sem(mymodel_v9_rev , data=west_w,
                            #group = "ECOREG_rev",
                            missing="ML", se="bootstrap")

summary(fit_v9_WEST_wade_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_wade_rev)
print(mi_min[mi_min$mi >3.0,])

# Residuals (difference in sample covariance matrix of obs and model-reproduced covariane matrix)
residuals(fit_v9_WEST_wade_rev)

#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_WEST_wade_rev<- capture.output(summary(fit_v9_WEST_wade_rev, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_WEST_wade_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_OE_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_WEST_wade_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_OE_rev_CI.csv",
          row.names = FALSE)

##################
# Save model fit parameters
## GRAB R2
r2_WEST<-parameterEstimates(fit_v9_WEST_wade_rev,rsquare=TRUE)%>%
  filter(op=="r2")

write.csv(r2_WEST,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_R2.csv",
          row.names=FALSE)

# GRAB OE R2 to be able to add to fit table
r2_red <- r2_WEST%>%
  filter(lhs=="OE_SCORE")%>%
  select(est)%>%
  mutate(across(where(is.numeric), round,2))

# TABLE OF FIT INDICES comparing 3 RESPONSES
table_fit <- matrix(NA, nrow=1, ncol=8)
colnames(table_fit) = c("Model","X2", "df","CFI","TLI", "RMSEA","SRMR","AIC")

table_fit[1,]<-c("OE",round(fitmeasures(fit_v9_WEST_wade_rev,
                                          c("chisq","df","cfi","tli",
                                            "rmsea","srmr","aic")),2))

# Add R2 for biotic response
table_fit<- data.frame(table_fit)%>%
  mutate(R2 = r2_red$est)

write.csv(table_fit, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_fit.csv",
          row.names = FALSE)


############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9<- lavaanPlot(model=fit_v9_WEST_wade_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_OE_rev_WESTw.png")


####################################
#########################
## PATH MODEL v9 - MMI - USING UNSCALED FIELD MEASURED PREDICTORS - NOT DEVIATION FROM REFERENCE
#  INCLUDING NATURAL FACTORS OMITTED IN THE OE DESIGNED MODEL
# Using MMI/100 to keep at scale similar to other predictors

mymodel_v9_MMI <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws + drought_mean
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + drought_mean + L_SLOPE_DPTH
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH + drought_mean
evap_index_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + drought_mean
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + drought_mean
Lpt01_XFC_NAT ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + L_SLOPE_DPTH + Tmax8110Ws + drought_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean

MMI_BENT_sc ~ Lpt01_XCMGW + LRBS_use + Lpt01_XFC_NAT + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_SLOPE_DPTH + Tmax8110Ws + drought_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF

'

fit_v9_WEST_MMI_wade <- sem(mymodel_v9_MMI , data=west_w,
                        #group = "ECOREG_rev",
                        missing="ML", se="bootstrap")

summary(fit_v9_WEST_MMI_wade, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_MMI_wade)
print(mi_min[mi_min$mi >3.0,])


#########################
## REVISED MMI MODEL - ITERATIVELY TRIMMING OUT n.s. pathways and predictors

# To match other models, removed some variables - did not greatly change model fit nor AIC
#d.excess_sc ~ + p7*PSUMPY_SY_WS_sc
# LRBS_use ~ + sd2*L_SLOPE_DPTH

# Indirect
# precip_mmi ~ #+ p7*e2*n1
#slope_mmi ~ sd2*r1 +

mymodel_v9_MMI_rev <- '
Lpt01_XCMGW ~ a2*asin_PCTAGR_WS + u3*asin_PCTURB_WS + ha3*W1_HAG + na2*W1_HNOAG + f3*asin_PCTFOR_WsRp100 + sd3*L_SLOPE_DPTH
LQLow_kmcl~ a4*asin_PCTAGR_WS + u5*asin_PCTURB_WS + p3*PSUMPY_SY_WS_sc + d2*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 + ha4*W1_HAG + na3*W1_HNOAG + t2*Tmax8110Ws + sd4*L_SLOPE_DPTH
LQbkf_kmcl ~ u6*asin_PCTURB_WS + p5*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio + ha5*W1_HAG + t3*Tmax8110Ws + sd5*L_SLOPE_DPTH + ph2*drought_mean
evap_index_sc ~  l5*LQLow_kmcl+ b4*LQbkf_kmcl + d4*L_NABD_NrmStorWs_ratio + f6*asin_PCTFOR_WsRp100
LRBS_use ~ u2*asin_PCTURB_WS + l2*LQLow_kmcl+ b2*LQbkf_kmcl + ha2*W1_HAG + f2*asin_PCTFOR_WsRp100 + p2*PSUMPY_SY_WS_sc
L_NTL ~ u7*asin_PCTURB_WS + x2*Lpt01_XCMGW + e2*evap_index_sc + na4*W1_HNOAG + f5*asin_PCTFOR_WsRp100 + w3*asin_PCTWET_WsRp100 + l4*LQLow_kmcl + p6*PSUMPY_SY_WS_sc
L_SULF ~ a3*asin_PCTAGR_WS + u4*asin_PCTURB_WS + f4*asin_PCTFOR_WsRp100 + l3*LQLow_kmcl+ b3*LQbkf_kmcl + p4*PSUMPY_SY_WS_sc

MMI_BENT_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + su1*L_SULF + l1*LQLow_kmcl + a1*asin_PCTAGR_WS + sd1*L_SLOPE_DPTH

# INDIRECT EFFECTS ON MMI
urb_mmi:= u2*r1 + u3*x1 + u4*su1 + u5*l1 + u5*l2*r1 +u5*l3*su1 + u5*l4*n1 + u5*l5*e2*n1 + u6*b2*r1 + u6*b3*su1 + u6*b4*e2*n1 + u7*n1
agr_mmi:= a2*x1 + a2*x2*n1 + a3*su1 + a4*l1 + a4*l2*r1 + a4*l3*su1 + a4*l4*n1 + a4*l5*e2*n1
dam_mmi:= d2*l1 + d2*l2*r1 + d2*l3*su1 + d2*l4*n1 + d2*l5*e2*n1 + d3*b2*r1 + d3*b3*su1 + d3*b4*e2*n1 + d4*e2*n1
tmax_mmi:= t2*l1 + t2*l2*r1 + t2*l3*su1 + t2*l4*n1 + t2*l5*e2*n1 + t3*b2*r1 + t3*b3*su1 + t3*b4*e2*n1
precip_mmi:= p2*r1 + p3*l1 + p3*l2*r1 + p3*l3*su1 + p3*l4*n1 + p3*l5*e2*n1 + p4*su1 + p5*b2*r1 + p5*b3*su1 + p5*b4*e2*n1 + p6*n1
phdi_mmi:= ph2*b2*r1 + ph2*b3*su1 + ph2*b4*e2*n1
slope_mmi:= sd3*x1 + sd3*x2*n1 + sd4*l1 + sd4*l2*r1 + sd4*l3*su1 + sd4*l4*n1 + sd4*l5*e2*n1 + sd5*b2*r1 + sd5*b3*su1 + sd5*b4*e2*n1
hag_mmi:= ha2*r1 + ha3*x1 + ha3*x2*n1 + ha4*l1 + ha4*l2*r1 + ha4*l3*su1 + ha4*l4*n1 + ha4*l5*e2*n1 + ha5*b2*r1 + ha5*b3*su1 + ha5*b4*e2*n1
nhag_mmi:= na2*x1 + na2*x2*n1 + na3*l1 + na3*l2*r1 + na3*l3*su1 + na3*l4*n1 + na3*l5*e2*n1 + na4*n1
rfor_mmi:=f2*r1 + f3*x1 + f3*x2*n1 + f4*su1 + f5*n1 + f6*e2*n1
rwet_mmi:= w2*l1 + w2*l2*r1 + w2*l3*su1 + w2*l4*n1 + w2*l5*e2*n1 + w3*n1
lflow_mmi:= l2*r1 + l3*su1 + l4*n1 + l5*e2*n1
bflow_mmi:= b2*r1 + b3*su1 + b4*e2*n1
dexcess_mmi:= e2*n1
xcmgw_mmi:= x2*n1

# TOTAL EFFECTS ON MMI
urb_tot:= urb_mmi
agr_tot:= a1 + agr_mmi
dam_tot:= dam_mmi
tmax_tot:= tmax_mmi
precip_tot:= precip_mmi
phdi_tot:= phdi_mmi
slope_tot:= sd1 + slope_mmi
hag_tot:= hag_mmi
nhag_tot:= nhag_mmi
rfor_tot:= rfor_mmi
rwet_tot:= rwet_mmi
lflow_tot:= l1 + lflow_mmi
bflow_tot:= bflow_mmi
dexcess_tot:= dexcess_mmi
xcmgw_tot:= x1 + xcmgw_mmi
rbs_tot:= r1
tn_tot:= n1
sulf_tot:= su1

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_NTL ~~                 L_SULF


'

fit_v9_WEST_MMI_wade_rev <- sem(mymodel_v9_MMI_rev , data=west_w,
                            #group = "ECOREG_rev",
                            missing="ML", se="bootstrap")

summary(fit_v9_WEST_MMI_wade_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_MMI_wade_rev)
print(mi_min[mi_min$mi >3.0,])


#############
# Export R output - MMI MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_WEST_MMI_wade_rev<- capture.output(summary(fit_v9_WEST_MMI_wade_rev, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_WEST_MMI_wade_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_MMI_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_WEST_MMI_wade_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_MMI_rev_CI.csv",
          row.names = FALSE)

##################
# Save model fit parameters
## GRAB R2
r2_WEST_MMI<-parameterEstimates(fit_v9_WEST_MMI_wade_rev,rsquare=TRUE)%>%
  filter(op=="r2")

write.csv(r2_WEST_MMI,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_MMI_R2.csv",
          row.names=FALSE)

# GRAB OE R2 to be able to add to fit table
r2_red_mmi <- r2_WEST_MMI%>%
  filter(lhs=="MMI_BENT_sc")%>%
  select(est)%>%
  mutate(across(where(is.numeric), round,2))

# TABLE OF FIT INDICES comparing 3 RESPONSES
table_fit <- matrix(NA, nrow=1, ncol=8)
colnames(table_fit) = c("Model","X2", "df","CFI","TLI", "RMSEA","SRMR","AIC")

table_fit[1,]<-c("MMI",round(fitmeasures(fit_v9_WEST_MMI_wade_rev,
                                          c("chisq","df","cfi","tli",
                                            "rmsea","srmr","aic")),2))

# Add R2 for biotic response
table_fit<- data.frame(table_fit)%>%
  mutate(R2 = r2_red_mmi$est)

write.csv(table_fit, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_MMI_fit.csv",
          row.names = FALSE)


############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9_mmi<- lavaanPlot(model=fit_v9_WEST_MMI_wade_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9_mmi,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_MMI_rev_WESTw.png")



####################################
#########################
## PATH MODEL v9 - EPT - USING UNSCALED FIELD MEASURED PREDICTORS - NOT DEVIATION FROM REFERENCE
#  INCLUDING NATURAL FACTORS OMITTED IN THE OE DESIGNED MODEL
# SCALED EPT by dividing by 100 to place on similar scale to other predictors
mymodel_v9_ept <- '
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + L_SLOPE_DPTH + Tmax8110Ws + drought_mean
LQLow_kmcl~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + Tmax8110Ws + drought_mean + L_SLOPE_DPTH
LQbkf_kmcl ~ asin_PCTAGR_WS + asin_PCTURB_WS + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTWET_WsRp100 +  W1_HAG + W1_HNOAG + Tmax8110Ws + L_SLOPE_DPTH + drought_mean
evap_index_sc ~  LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100 + Tmax8110Ws + L_SLOPE_DPTH + drought_mean
LRBS_use ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + PSUMPY_SY_WS_sc + L_SLOPE_DPTH + drought_mean
Lpt01_XFC_NAT ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW  + LQLow_kmcl+ LQbkf_kmcl + W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + L_SLOPE_DPTH + Tmax8110Ws + drought_mean
L_PTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_NTL ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_CHLR ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_SULF ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean
L_TURB ~ asin_PCTAGR_WS + asin_PCTURB_WS + Lpt01_XCMGW + evap_index_sc +  W1_HAG + W1_HNOAG + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + LQLow_kmcl+ LQbkf_kmcl + PSUMPY_SY_WS_sc + drought_mean

EPT_RICH_sc ~ Lpt01_XCMGW + LRBS_use + Lpt01_XFC_NAT + L_PTL + L_NTL + L_CHLR + L_SULF + L_TURB + LQLow_kmcl+ LQbkf_kmcl + asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + asin_PCTWET_WsRp100 + W1_HAG + W1_HNOAG + L_SLOPE_DPTH + Tmax8110Ws + drought_mean

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_PTL ~~ L_NTL
L_CHLR ~~ L_SULF


'

fit_v9_WEST_wade_ept <- sem(mymodel_v9_ept , data=west_w,
                        #group = "ECOREG_rev",
                        missing="ML", se="bootstrap")

summary(fit_v9_WEST_wade_ept, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_wade_ept)
print(mi_min[mi_min$mi >3.0,])


#########################
## PATH MODEL v9 - EPT REVISED - DROPPING ns relationships p>0.1
# Revising to make models more similar among biotic indices
#L_NTL~a5*asin_PCTAGR_WS +
# Indirect agr_ept ~ + a5*n1
# And added asin_PCTWET_WsRp100 + W1_HNOAG to TN model

mymodel_v9_ept_rev <- '
Lpt01_XCMGW ~ a2*asin_PCTAGR_WS + u4*asin_PCTURB_WS + ha3*W1_HAG + na2*W1_HNOAG + f3*asin_PCTFOR_WsRp100 + sd2*L_SLOPE_DPTH
LQLow_kmcl~ a3*asin_PCTAGR_WS + u3*asin_PCTURB_WS + p3*PSUMPY_SY_WS_sc + d2*L_NABD_NrmStorWs_ratio + w2*asin_PCTWET_WsRp100 + ha4*W1_HAG + na3*W1_HNOAG + t2*Tmax8110Ws + sd3*L_SLOPE_DPTH
LQbkf_kmcl ~ u5*asin_PCTURB_WS + p4*PSUMPY_SY_WS_sc + d3*L_NABD_NrmStorWs_ratio +  ha5*W1_HAG + t3*Tmax8110Ws + sd4*L_SLOPE_DPTH + ph2*drought_mean
evap_index_sc ~ l5*LQLow_kmcl+ b4*LQbkf_kmcl + d4*L_NABD_NrmStorWs_ratio + f6*asin_PCTFOR_WsRp100
LRBS_use ~ u2*asin_PCTURB_WS + l2*LQLow_kmcl+ b2*LQbkf_kmcl + ha2*W1_HAG + f2*asin_PCTFOR_WsRp100 + p2*PSUMPY_SY_WS_sc
L_NTL ~ u7*asin_PCTURB_WS + x2*Lpt01_XCMGW + e2*evap_index_sc + f5*asin_PCTFOR_WsRp100 + l4*LQLow_kmcl + p6*PSUMPY_SY_WS_sc + w3*asin_PCTWET_WsRp100 + na4*W1_HNOAG
L_SULF ~ a4*asin_PCTAGR_WS + u6*asin_PCTURB_WS + f4*asin_PCTFOR_WsRp100 + l3*LQLow_kmcl + b3*LQbkf_kmcl + p5*PSUMPY_SY_WS_sc

EPT_RICH_sc ~ x1*Lpt01_XCMGW + r1*LRBS_use + n1*L_NTL + su1*L_SULF + l1*LQLow_kmcl + f1*asin_PCTFOR_WsRp100 + sd1*L_SLOPE_DPTH + p1*PSUMPY_SY_WS_sc

# INDIRECT EFFECTS ON EPT
urb_ept:= u2*r1 + u3*l1 + u3*l2*r1 + u3*l3*su1 + u3*l4*n1 + u3*l5*e2*n1 + u4*x1 + u4*x2*n1 + u5*b2*r1 + u5*b3*su1 + u5*b4*e2*n1 + u6*su1 + u7*n1
agr_ept:= a2*x1 + a2*x2*n1 + a3*l1 + a3*l2*r1 + a3*l3*su1 + a3*l4*n1 + a3*l5*e2*n1 + a4*su1
dam_ept:= d2*l1 + d2*l2*r1 + d2*l3*su1 + d2*l4*n1 + d2*l5*e2*n1 + d3*b2*r1 + d3*b3*su1 + d3*b4*e2*n1
tmax_ept:= t2*l1 + t2*l2*r1 + t2*l3*su1 + t2*l4*n1 + t2*l5*e2*n1 + t3*b2*r1 + t3*b3*su1 + t3*b4*e2*n1
precip_ept:= p2*r1 + p3*l1 + p3*l2*r1 + p3*l3*su1 + p3*l4*n1 + p3*l5*e2*n1 + p4*b2*r1 + p4*b3*su1 + p4*b4*e2*n1
phdi_ept:= ph2*b2*r1 + ph2*b3*su1 + ph2*b4*e2*n1
slope_ept:= sd2*x1 + sd2*x2*n1 + sd3*l1 + sd3*l2*r1 + sd3*l3*su1 + sd3*l4*n1 + sd3*l5*e2*n1 + sd4*b2*r1 + sd4*b3*su1 + sd4*b4*e2*n1
hag_ept:= ha2*r1 + ha3*x1 + ha3*x2*n1 + ha4*l1 + ha4*l2*r1 + ha4*l3*su1 + ha4*l4*n1 + ha4*l5*e2*n1 + ha5*b2*r1 + ha5*b3*su1 + ha5*b4*e2*n1
nhag_ept:= na2*x1 + na2*x2*n1 + na3*l1 + na3*l2*r1 + na3*l3*su1 + na3*l4*n1 + na3*l5*e2*n1 + na4*n1
rfor_ept:= f2*r1 + f3*x1 + f3*x2*n1 + f4*su1 + f5*n1 + f6*e2*n1
rwet_ept:= w2*l1 + w2*l2*r1 + w2*l3*su1 + w2*l4*n1 + w2*l5*e2*n1 + w3*n1
lflow_ept:= l2*r1 + l3*su1 + l4*n1 + l5*e2*n1
bflow_ept:= b2*r1 + b3*su1 + b4*e2*n1
dexcess_ept:= e2*n1
xcmgw_ept:= x2*n1

# TOTAL EFFECTS ON EPT
urb_tot:= urb_ept
agr_tot:= agr_ept
dam_tot:= dam_ept
tmax_tot:= tmax_ept
precip_tot:= p1 + precip_ept
phdi_tot:= phdi_ept
slope_tot:= sd1 + slope_ept
hag_tot:= hag_ept
nhag_tot:= nhag_ept
rfor_tot:= f1 + rfor_ept
rwet_tot:= rwet_ept
lflow_tot:= l1 + lflow_ept
bflow_tot:= bflow_ept
dexcess_tot:= dexcess_ept
xcmgw_tot:= x1 + xcmgw_ept
rbs_tot:= r1
tn_tot:= n1
sulf_tot:= su1

# Covariance
LQLow_kmcl~~LQbkf_kmcl
L_NTL ~~                 L_SULF


'

fit_v9_WEST_wade_ept_rev <- sem(mymodel_v9_ept_rev , data=west_w,
                            #group = "ECOREG_rev",
                            missing="ML", se="bootstrap")

summary(fit_v9_WEST_wade_ept_rev, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v9_WEST_wade_ept_rev)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - ORIGINAL MODEL
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_v9_WEST_wade_ept_rev<- capture.output(summary(fit_v9_WEST_wade_ept_rev, standardized=FALSE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_v9_WEST_wade_ept_rev, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_EPT_rev.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_v9_WEST_wade_ept_rev)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_EPT_rev_CI.csv",
          row.names = FALSE)

##################
# Save model fit parameters
## GRAB R2
r2_ept_WEST<-parameterEstimates(fit_v9_WEST_wade_ept_rev,rsquare=TRUE)%>%
  filter(op=="r2")%>%
  select(-c(rhs,label,se,z,pvalue,ci.lower,ci.upper))

write.csv(r2_ept_WEST,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_EPT_R2.csv",
          row.names=FALSE)

# GRAB OE R2 to be able to add to fit table
r2_ept_red <- r2_ept_WEST%>%
  filter(lhs=="EPT_RICH_sc")%>%
  select(est)%>%
  mutate(across(where(is.numeric), round,2))

# TABLE OF FIT INDICES
table_fit_ept <- matrix(NA, nrow=1, ncol=8)
colnames(table_fit_ept) = c("Model","X2", "df","CFI","TLI", "RMSEA","SRMR","AIC")

table_fit_ept[1,]<-c("EPT",round(fitmeasures(fit_v9_WEST_wade_ept_rev,
                                          c("chisq","df","cfi","tli",
                                            "rmsea","srmr","aic")),2))

table_fit_ept<- data.frame(table_fit_ept)%>%
  mutate(R2 = r2_ept_red$est)

write.csv(table_fit_ept, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WESTw_m9_EPT_fit.csv",
          row.names = FALSE)

############
# LOOK AT MODEL USING DIAGRAM PACKAGES
p9_ept<- lavaanPlot(model=fit_v9_WEST_wade_ept_rev, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(p9_ept,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/Diag_m9_EPT_rev_WESTw.png")


