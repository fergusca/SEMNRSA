################################
## COMPARING ANALYTIC FRAMEWORK COEFFICIENTS
##  Examining coefficients from three modeling approaches: univariate regression, MLR, and SEM
##  Want to see how coefficient estimates vary among the three methods
##  Performed on WEST (WMT + XER) for O/E

##  8/12/2022
## 2/3/2023 - updated to model v15 and ran on WMT

################################

remove(list=ls())

library(tidyverse)
#library(tidyr)
library(dplyr)
library(dotwhisker)

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

############################
# EXERCISE OF RUNNING LINEAR REGRESSION AND MULTIPLE REGRESSION TO COMPARE WITH SEM OUTPUT
# Dot whisker plot of regression coefficients https://cran.r-project.org/web/packages/dotwhisker/vignettes/dotwhisker-vignette.html

# WEST Data subset to scale and run regressions
westw_red<-west_w %>%
  select(OE_SCORE, L_NTL,PSUMPY_SY_WS_sc, Lpt01_XCMGW, LQLow_kmcl, evap_index_sc, asin_PCTURB_WS,W1_HNOAG,asin_PCTFOR_WsRp100,
         L_SLOPE_DPTH,W1_HAG,asin_PCTAGR_WS, # drivers of XCMGW
         Tmax8110Ws, L_NABD_NrmStorWs_ratio,asin_PCTWET_WsRp100, # additional drivers of lflow
         LQbkf_kmcl, drought_mean,LRBS_use,L_SULF)

# Scale data
westw_sc <- westw_red %>%
  mutate_each(funs(scale))

# Check if means are close to zero
colMeans(westw_sc)

# Univariate regression
# TN
m1 <- westw_sc %>%
  do(broom::tidy(lm(OE_SCORE~L_NTL,data=.))) %>%
  #rename(model=am)%>%
  relabel_predictors(c(
    L_NTL = "TN"))%>%
  mutate(model="Univariate")

# RIparian forest
m1 <- westw_sc %>%
  do(broom::tidy(lm(OE_SCORE~asin_PCTFOR_WsRp100,data=.))) %>%
  #rename(model=am)%>%
  relabel_predictors(c(
    asin_PCTFOR_WsRp100="Forest Rp"))%>%
  mutate(model="Univariate")
m1

summary(lm(OE_SCORE~L_NTL,data=westw_sc))

#Residual standard error: 0.9094 on 590 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.1744,	Adjusted R-squared:  0.173
#F-statistic: 124.6 on 1 and 590 DF,  p-value: < 2.2e-16

# Multiple regression
m2 <- westw_sc %>%
  do(broom::tidy(lm(OE_SCORE~asin_PCTAGR_WS+asin_PCTURB_WS+L_NABD_NrmStorWs_ratio+
                      PSUMPY_SY_WS_sc+Tmax8110Ws+drought_mean+
                      LQbkf_kmcl+LQLow_kmcl+evap_index_sc+L_SLOPE_DPTH+
                      W1_HAG+W1_HNOAG+
                      asin_PCTFOR_WsRp100+asin_PCTWET_WsRp100+Lpt01_XCMGW+
                      LRBS_use+L_NTL+L_SULF, data= .))) %>%
  #rename(model=am)%>%
  relabel_predictors(c(asin_PCTAGR_WS="Agr Ws",asin_PCTURB_WS="Urban Ws",L_NABD_NrmStorWs_ratio="Dam",
                       PSUMPY_SY_WS_sc="Precipitation",Tmax8110Ws="Max Temp",drought_mean="Drought index",
                       LQbkf_kmcl="Bankfull flow",LQLow_kmcl="Summer flow",evap_index_sc="Evaporation indicator",
                       L_SLOPE_DPTH="Slope*depth",W1_HAG="Agr index Rp", W1_HNOAG="Non-agr index Rp",
                       asin_PCTFOR_WsRp100="Forest Rp",asin_PCTWET_WsRp100="Wetland Rp",Lpt01_XCMGW="Riparian cover",
                       LRBS_use="Bed stability",L_NTL="TN",L_SULF="Sulfate"))%>%
  mutate(model="Multiple reg")

m2

summary(lm(OE_SCORE~asin_PCTAGR_WS+asin_PCTURB_WS+L_NABD_NrmStorWs_ratio+
                PSUMPY_SY_WS_sc+Tmax8110Ws+drought_mean+
                LQbkf_kmcl+LQLow_kmcl+evap_index_sc+L_SLOPE_DPTH+
                W1_HAG+W1_HNOAG+
                asin_PCTFOR_WsRp100+asin_PCTWET_WsRp100+Lpt01_XCMGW+
                LRBS_use+L_NTL+L_SULF, data= westw_sc))

# READ IN SEM bootstrap OE Model
m3<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WEST/WESTw_m9_OE_rev_CI.csv")
# Modify SEM output to get dotplot
m3_mod<- m3%>%
  filter(str_detect(label,"_tot"))%>%
  rename(term=label,
         estimate=est.std,
         std.error=se,
         conf.low= ci.lower,
         conf.high=ci.upper,
         statistic=z,
         p.value=pvalue)%>%
  mutate(term = recode_factor(term, agr_tot="Agr Ws",urb_tot="Urban Ws",dam_tot="Dam",
                              precip_tot="Precipitation",tmax_tot="Max Temp",phdi_tot="Drought index",
                              bflow_tot="Bankfull flow",lflow_tot="Summer flow",dexcess_tot="Evaporation indicator",
                              slope_tot="Slope*depth",
                              hag_tot="Agr index Rp", nhag_tot="Non-agr index Rp",rfor_tot="Forest Rp",rwet_tot="Wetland Rp",
                              xcmgw_tot="Riparian cover",rbs_tot="Bed stability", tn_tot="TN", sulf_tot="Sulfate"))%>%

  mutate(model="SEM")%>%
  select(term,estimate,std.error,statistic,p.value,model)

### REGRESSION DOT DIAGRAMS
#library(dotwhisker)

# Bring model output together
m_set<-rbind(m1, m2,m3_mod)
coeff_dotplot<-dwplot(m_set)+
  theme_bw()+
  ggtitle("Std. model coeff: WEST wade O/E")

coeff_dotplot
#dwplot(m_set,style="distribution") # Looks crazy because very high peaks for SEM

# PRINT Coefficient Dot and whisker plot
# OE WEST
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Dot_whisker_WEST_OE.tiff",
     width=5, height = 6, units="in", res=200)
coeff_dotplot
dev.off()



##############################
## MODEL v15 WMT wadeable - with specific stream power and evaporation
# WMTw Data subset to scale and run regressions
wmtw_red<-wmt_w %>%
  #filter(!is.na(OE_SCORE))%>%
  select(OE_SCORE, MMI_BENT, asin_PCTURB_WS, asin_PCTAGR_WS,L_NABD_NrmStorWs_ratio,
         PSUMPY_SY_WS_sc, drought_mean,
         L_STRM_POWER, LQLow_kmcl,LQbkf_kmcl, evap_index_sc,
         Lpt01_XCMGW,  W1_HAG,W1_HNOAG,
         asin_PCTNATTERR_WsRp100,asin_PCTWET_WsRp100, # drivers of XCMGW
         LRBS_use,L_NTL,L_SULF)

# Scale data
wmtw_sc <- wmtw_red %>%
  mutate_each(funs(scale))

# Check if means are close to zero
colMeans(wmtw_sc)

# Univariate regression
# TN
m1_wmt <- wmtw_sc %>%
  do(broom::tidy(lm(OE_SCORE~L_NTL,data=.))) %>%
  #rename(model=am)%>%
  relabel_predictors(c(
    L_NTL = "TN"))%>%
  mutate(model="Univariate")

# RIparian forest
#m1 <- westw_sc %>%
#  do(broom::tidy(lm(OE_SCORE~asin_PCTFOR_WsRp100,data=.))) %>%
  #rename(model=am)%>%
#  relabel_predictors(c(
#    asin_PCTFOR_WsRp100="Forest Rp"))%>%
#  mutate(model="Univariate")
m1_wmt

summary(lm(OE_SCORE~L_NTL,data=wmtw_sc))

#Residual standard error: 0.9094 on 590 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.1744,	Adjusted R-squared:  0.173
#F-statistic: 124.6 on 1 and 590 DF,  p-value: < 2.2e-16

# Multiple regression
m2_wmt <- wmtw_sc %>%
  do(broom::tidy(lm(OE_SCORE~asin_PCTURB_WS+L_NABD_NrmStorWs_ratio+
                      PSUMPY_SY_WS_sc+
                      LQbkf_kmcl+LQLow_kmcl+evap_index_sc+L_STRM_POWER+
                      W1_HAG+
                      asin_PCTNATTERR_WsRp100+asin_PCTWET_WsRp100+Lpt01_XCMGW+
                      LRBS_use+L_NTL, data= .))) %>%
  #rename(model=am)%>%
  relabel_predictors(c(asin_PCTURB_WS="Urban Ws",L_NABD_NrmStorWs_ratio="Dam",
                       PSUMPY_SY_WS_sc="Precipitation",drought_mean="Drought index",
                       LQbkf_kmcl="Bankfull flow",LQLow_kmcl="Summer flow",evap_index_sc="Evaporation indicator",
                       L_STRM_POWER="Specific stream power",W1_HAG="Agr index Rp",
                       asin_PCTNATTERR_WsRp100="Terrestrial cover Rp",asin_PCTWET_WsRp100="Wetland Rp",Lpt01_XCMGW="Riparian site index",
                       LRBS_use="Bed stability",L_NTL="TN"))%>%
  mutate(model="Multiple reg")

m2_wmt

summary(lm(OE_SCORE~asin_PCTURB_WS+L_NABD_NrmStorWs_ratio+
             PSUMPY_SY_WS_sc+
             LQbkf_kmcl+LQLow_kmcl+evap_index_sc+L_STRM_POWER+
             W1_HAG+
             asin_PCTNATTERR_WsRp100+asin_PCTWET_WsRp100+Lpt01_XCMGW+
             LRBS_use+L_NTL, data= wmtw_sc))

# READ IN SEM bootstrap OE Model
m3_wmt<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_OE_CI.csv")
# Modify SEM output to get dotplot
m3_mod_wmt<- m3_wmt%>%
  filter(str_detect(label,"_tot"))%>%
  rename(term=label,
         estimate=est.std,
         std.error=se,
         conf.low= ci.lower,
         conf.high=ci.upper,
         statistic=z,
         p.value=pvalue)%>%
  mutate(term = recode_factor(term, agr_tot="Agr Ws",urb_tot="Urban Ws",dam_tot="Dam",
                              precip_tot="Precipitation",tmax_tot="Max Temp",phdi_tot="Drought index",
                              bflow_tot="Bankfull flow",lflow_tot="Summer flow",dexcess_tot="Evaporation indicator",
                              strpwr_tot="Specific stream power",
                              hag_tot="Agr index Rp", nhag_tot="Non-agr index Rp",rnat_tot="Terrestrial cover Rp",rwet_tot="Wetland Rp",
                              xcmgw_tot="Riparian site index",rbs_tot="Bed stability", tn_tot="TN", sulf_tot="Sulfate"))%>%

  mutate(model="SEM")%>%
  select(term,estimate,std.error,statistic,p.value,model)

### REGRESSION DOT DIAGRAMS
#library(dotwhisker)

# Bring model output together
m_set_wmt<-rbind(m1_wmt, m2_wmt,m3_mod_wmt)
coeff_dotplot_wmt<-dwplot(m_set_wmt)+
  theme_bw()+
  ggtitle("Std. model coeff: WMT wade O/E")

coeff_dotplot_wmt
#dwplot(m_set,style="distribution") # Looks crazy because very high peaks for SEM

# PRINT Coefficient Dot and whisker plot
# OE WMT v15
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Dot_whisker_WMT_v15_OE.tiff",
     width=5, height = 6, units="in", res=200)
coeff_dotplot_wmt
dev.off()


#############################
#####################
## WMT v15 MMI
# Univariate regression
# TN
m1_wmt_mmi <- wmtw_sc %>%
  do(broom::tidy(lm(MMI_BENT~L_NTL,data=.))) %>%
  #rename(model=am)%>%
  relabel_predictors(c(
    L_NTL = "TN"))%>%
  mutate(model="Univariate")

m1_wmt_mmi

summary(lm(MMI_BENT~L_NTL,data=wmtw_sc))


# Multiple regression
m2_wmt_mmi <- wmtw_sc %>%
  do(broom::tidy(lm(MMI_BENT~asin_PCTURB_WS+L_NABD_NrmStorWs_ratio+
                      PSUMPY_SY_WS_sc+drought_mean+
                      LQbkf_kmcl+LQLow_kmcl+evap_index_sc+L_STRM_POWER+
                      W1_HAG+
                      asin_PCTNATTERR_WsRp100+asin_PCTWET_WsRp100+Lpt01_XCMGW+
                      LRBS_use+L_NTL, data= .))) %>%
  #rename(model=am)%>%
  relabel_predictors(c(asin_PCTURB_WS="Urban Ws",L_NABD_NrmStorWs_ratio="Dam",
                       PSUMPY_SY_WS_sc="Precipitation",drought_mean="Drought index",
                       LQbkf_kmcl="Bankfull flow",LQLow_kmcl="Summer flow",evap_index_sc="Evaporation indicator",
                       L_STRM_POWER="Specific stream power",W1_HAG="Agr index Rp",
                       asin_PCTNATTERR_WsRp100="Terrestrial cover Rp",asin_PCTWET_WsRp100="Wetland Rp",Lpt01_XCMGW="Riparian site index",
                       LRBS_use="Bed stability",L_NTL="TN"))%>%
  mutate(model="Multiple reg")

m2_wmt_mmi

summary(lm(MMI_BENT~asin_PCTURB_WS+L_NABD_NrmStorWs_ratio+
             PSUMPY_SY_WS_sc+
             LQbkf_kmcl+LQLow_kmcl+evap_index_sc+L_STRM_POWER+
             W1_HAG+
             asin_PCTNATTERR_WsRp100+asin_PCTWET_WsRp100+Lpt01_XCMGW+
             LRBS_use+L_NTL, data= wmtw_sc))

# READ IN SEM bootstrap OE Model
m3_wmt_mmi<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/WMTw_m15_MMI_CI.csv")
# Modify SEM output to get dotplot
m3_mod_wmt_mmi<- m3_wmt_mmi%>%
  filter(str_detect(label,"_tot"))%>%
  rename(term=label,
         estimate=est.std,
         std.error=se,
         conf.low= ci.lower,
         conf.high=ci.upper,
         statistic=z,
         p.value=pvalue)%>%
  mutate(term = recode_factor(term, agr_tot="Agr Ws",urb_tot="Urban Ws",dam_tot="Dam",
                              precip_tot="Precipitation",tmax_tot="Max Temp",phdi_tot="Drought index",
                              bflow_tot="Bankfull flow",lflow_tot="Summer flow",dexcess_tot="Evaporation indicator",
                              strpwr_tot="Specific stream power",
                              hag_tot="Agr index Rp", nhag_tot="Non-agr index Rp",rnat_tot="Terrestrial cover Rp",rwet_tot="Wetland Rp",
                              xcmgw_tot="Riparian site index",rbs_tot="Bed stability", tn_tot="TN", sulf_tot="Sulfate"))%>%

  mutate(model="SEM")%>%
  select(term,estimate,std.error,statistic,p.value,model)

### REGRESSION DOT DIAGRAMS
#library(dotwhisker)

# Bring model output together
m_set_wmt_mmi<-rbind(m1_wmt_mmi, m2_wmt_mmi,m3_mod_wmt_mmi)
coeff_dotplot_wmt_mmi<-dwplot(m_set_wmt_mmi)+
  theme_bw()+
  ggtitle("Std. model coeff: WMT wade MMI")

coeff_dotplot_wmt_mmi
#dwplot(m_set,style="distribution") # Looks crazy because very high peaks for SEM

# PRINT Coefficient Dot and whisker plot
# MMI WMT v15
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Figures/Dot_whisker_WMT_v15_MMI.tiff",
     width=5, height = 6, units="in", res=200)
coeff_dotplot_wmt_mmi
dev.off()
