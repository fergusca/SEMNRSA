################
## COMPILE DATA
##  Bring together three NRSA surveys data 08-09; 13-14; 18-19
##  Data have been compiled and processed for each respective survey year
##  See R script for processing steps in the load_dataXX.R scripts
##
## 3/16/2022
## 5/26/2022 - added PHDI
## 6/24/2022 - Added Omernik level III and IV ecoregion codes
## 9/2/2022 - Added Rd density & human population
## 9/6/2022 - Added O and E
## 2/6/2023 - Added riparian understory and groundcover (XMW, XGW, XGB)
## 3/14/2023 - Added HUC8 to compiled dataset
###########

rm(list=ls())

# Libraries
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyr)
library(tidyverse)
library(ggpubr) # statistics for comparison
library(reshape2)

###############
## READ PROCESSED NRSA-STREAMCAT DATA - vars = 190 or 188
##  Processed data are in the R project (C:drive)
# NRSA 2008-09 n = 2303
nrsa0809 <- read.csv("data_processed/nrsa0809/nrsa0809_to_merge.csv")

# NRSA 2013-14 n = 2261
nrsa1314 <- read.csv("data_processed/nrsa1314/nrsa1314_to_merge.csv")

# NRSA 2018-19 n = 2110
nrsa1819 <- read.csv("data_processed/nrsa1819/nrsa1819_to_merge.csv")


##################
## PROCESS DATA TO MERGE TOGETHER
# Correct state abbreviations - can use base R function
nrsa1819<-nrsa1819 %>%
  mutate(STATE = state.abb[match(STATE_NM, state.name)])
table(nrsa1819$STATE)

# CONVERT NTL from ug/L to mg/L in NRSA0809
nrsa0809$NTL<-nrsa0809$NTL/1000
summary(nrsa0809$NTL)

# Need to remove benthic MMI and Old OE from 0809 and 1314 surveys
#names(nrsa0809)
#names(nrsa1314)

nrsa0809<-nrsa0809%>%
  select(-c("STRAHLERORDER","MMI_BENT","OE_SCORE_OLD"))

nrsa1314<-nrsa1314%>%
  select(-c("STRAH_ORD","MMI_BENT","OE_SCORE_OLD"))

nrsa1819<-nrsa1819%>%
  select(-c("STATE_NM","STRAH_ORD"))

# REORDER NRSA1819 variables
nrsa1819<-nrsa1819%>%
  select(c("UID","SITE_ID","VISIT_NO","DATE_COL","YEAR","SITETYPE","STATE","AG_ECO3","AG_ECO9","AG_ECO5",
           "US_L3CODE","US_L4CODE","HUC8",
           "LAT_DD83","LON_DD83","PROTOCOL","REALM",#"STRAH_ORD",
           "OE_SCORE",
           "AMMONIA_N_RESULT","ANC_RESULT","CHLORIDE_RESULT","COLOR_RESULT","COND_RESULT","DOC_RESULT","MAGNESIUM_RESULT","SODIUM_RESULT",
           "POTASSIUM_RESULT","NITRATE_N_RESULT","NITRITE_N_RESULT","NTL_RESULT","PTL_RESULT","SULFATE_RESULT","TSS_RESULT","TURB_RESULT",
           "RT_WQI","CL_pt","SO4_pt","PTL_pt","NTL_pt","TURB_pt","ENTERO_PT", "DO_PT","PH_PT","WQII",
           "H2O_dD","H2O_d18O","d.excess","MAST_SY","MSST_SY","MWST_SY",
           "LDCBF_G08",
           "XDEPTH_CM","SDDEPTH_CM","XWXD","RP100","XBKF_W","XBKF_H","XINC_H","SINU","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SAFN","PCT_SFGF","LDMB_BW5","LRBS_BW5","LRBS_G08","PCT_FAST","PCT_SLOW",
           "RpRat","PCT_BDRK","XFC_ALG","XFC_AQM","XFC_LWD","XFC_NAT","V1W_MSQ","XCDENBK","XCDENMID",
           "XCL","XGB","XGW","XMW","XC","XCMGW","QR1","QRVeg1","RDIST1","W1_HALL","W1H_WALL","W1_HNOAG","W1_HAG",
           "W1H_CROP","XSLOPE_use","XWIDTH_use",
           "Lpt01_XCMGW","Lpt01_XFC_NAT","LRBS_use",
           "RDIST_COND","LRBS_Cond_use","LOE_RBS_use",
           "LXCMGW_Cond_use","LOE_XCMGW_use","LXFC_NAT_Cond_use","LOE_XFC_NAT_use",
           "Qbkf_cl","LQbkf_cl","Qbkf_kmcl","LQbkf_kmcl",
           "QLow_cl","LQLow_cl","QLow_kmcl","LQLow_kmcl",
           "LOE_QLow_cl","LOE_Qbkf_cl",
           "CatAreaSqKm","WsAreaSqKm","CatAreaSqKmRp100","WsAreaSqKmRp100",
           "PSUMPY_SY_PT","PSUMPY_SY_WS","TMEAN_PWSY_PT","TMEAN_PWSY_WS",
           "TMEAN_WSY_PT","TMEAN_WSY_WS","TMEAN_SSY_PT","TMEAN_SSY_WS",
           "TMEAN_SY_PT","TMEAN_SY_WS","PMAXWs","PMINWs","Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws",
           "BFIWs","ElevWs","PermWs","RunoffWs","HydrlCondWs","WDMAX_PT","WDMIN_PT","WDSUM_PT",
           "PCTOW_WS", "PCTICE_WS", "PCTURBOP_WS", "PCTURBLO_WS", "PCTURBMD_WS", "PCTURBHI_WS",
           "PCTDECID_WS", "PCTCONIF_WS", "PCTMXFST_WS", "PCTSHRB_WS", "PCTGRS_WS", "PCTHAY_WS", "PCTCROP_WS",
           "PCTWDWET_WS", "PCTHBWET_WS", "PCTOW_WsRp100", "PCTICE_WsRp100", "PCTURBOP_WsRp100", "PCTURBLO_WsRp100",
           "PCTURBMD_WsRp100","PCTURBHI_WsRp100", "PCTDECID_WsRp100", "PCTCONIF_WsRp100", "PCTMXFST_WsRp100",
           "PCTSHRB_WsRp100", "PCTGRS_WsRp100", "PCTHAY_WsRp100", "PCTCROP_WsRp100",
           "PCTWDWET_WsRp100", "PCTHBWET_WsRp100", "PCTIMP_WS", "PCTIMP_WsRp100",
           "NABD_DensWs","NABD_NIDStorWs","NABD_NrmStorWs",
           "RdDensWs","RdDensWsRp100",
           "PopDen2010Ws","PopDen2010WsRp100",
           "AgKffactWs","FertWs","ManureWs","NPDESDensWs","NPDESDensWsRp100"))

names(nrsa1819) # dropped "XWIDTH",
# Convert VISIT_NO from integer to character
#nrsa0809$VISIT_NO<-as.character(nrsa0809$VISIT_NO)
#nrsa1314$VISIT_NO<-as.character(nrsa1314$VISIT_NO)
nrsa1819$VISIT_NO<- as.integer(nrsa1819$VISIT_NO)
table(nrsa1819$VISIT_NO)

# Make TSS a string
nrsa1819$TSS_RESULT<- as.double(nrsa1819$TSS_RESULT)
str(nrsa1819$TSS_RESULT)

## HAVE COLUMN NAMES MATCH FOR ALL SURVEYS
# Check order
var0809<-names(nrsa0809)
var1314<-names(nrsa1314)
var1819<-names(nrsa1819)
write(var0809, "data_processed/var0809_check.csv")
write(var1314, "data_processed/var1314_check.csv")
write(var1819, "data_processed/var1819_check.csv")

##############
# RELABEL column names in nrsa0809 to match nrsa1314 column names
## nrsa1314 and nrsa1819 column names match already
names(nrsa0809)<- var1314
names(nrsa0809)
names(nrsa1819)<-var1314

# CHange from character to integer
nrsa0809$LDCBF_G08 <- as.numeric(nrsa0809$LDCBF_G08)
str(nrsa0809$LDCBF_G08)
head(nrsa0809$LDCBF_G08)

# Nitrite has scientific notation (7.0e-04) so values are treated as character
nrsa1819$NITRITE_N_RESULT<-as.numeric(nrsa1819$NITRITE_N_RESULT)
head(nrsa1819$NITRITE_N_RESULT)
str(nrsa1819$NITRITE_N_RESULT)

# Change HUC8 from integer to character
nrsa0809$HUC8<- as.character(nrsa0809$HUC8)

## THREE DATASETS HAVE 187 variables in matching order

########################
## COMBINE DATASETS - n =6674 obs with 188 vars
all_dat<-bind_rows(nrsa0809=nrsa0809, nrsa1314=nrsa1314, nrsa1819=nrsa1819,
                .id="nrsa_survey")
table(all_dat$nrsa_survey)
#nrsa0809 nrsa1314 nrsa1819
#2303     2261     2110

# GRAB UNIQUE ID THAT CAN BE USED TO CROSSWALK AMONG THREE SURVEYS - IN WQII
wqii_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/WQII/wqii_nrsa.csv")
wqii<-wqii_org%>%
  select(c("SITE_ID","VISIT_NO","YEAR","UNIQUE_ID"))

# Merge compiled NRSA and WQII subset together using left_join
all_dat_wid<-left_join(all_dat,wqii,
                          by=c("SITE_ID","VISIT_NO","YEAR"))
names(all_dat_wid)

# Drop duplicates - goes back to 6674
#all_dat_id=all_dat_id%>%
#  distinct(SITE_ID,VISIT_NO, .keep_all=TRUE)

table(all_dat_wid$nrsa_survey)
# nrsa0809 nrsa1314 nrsa1819
# 2303     2261     2110


#####################
## GRAB OMERNIK ECOREGION NAMES
# NRSA 2013-14
site_org <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_siteinformation_wide_04292019.csv")
# GET DISTINCT LEVEL III
l3<-site_org%>%
  distinct(US_L3CODE, .keep_all=TRUE)%>%
  select(US_L3CODE, US_L3NAME)
# GET DISTINCT LEVEL IV
l4<-site_org%>% # n=577
  distinct(US_L4CODE, .keep_all=TRUE)%>%
  select(US_L4CODE, US_L4NAME)

# MERGE ECOREGION NAME COLUMNS INTO DATASET
all_dat_id3<- left_join(all_dat_wid,l3,
                       by="US_L3CODE")
all_dat_id <- left_join(all_dat_id3, l4,
                        by="US_L4CODE")

######################
## SUBSET DATA
## ALL VISIT =1; Drop observations from sites that were resampled in subsequent surveys
#   1) Retain all the 0809 sites (VISIT_NO = 1)
#   2) Retain all new sites 1314 (VISIT_NO = 1) - drop any 1314 sites that were in 0809
#   3) Retain all new sites 1819 (VISIT_NO = 1) - drop any 1819 sites that were in either 0809 and/or 1314


length(unique(nrsa0809$SITE_ID)) #n =2123
length(unique(nrsa1314$SITE_ID)) #n = 2069
length(unique(nrsa1819$SITE_ID)) # n = 1919

## SEE HOW MANY SITES WERE VISITED MORE THAN ONCE
#   both within and across surveys - 3649
duplic<-all_dat_id%>%
  group_by(UNIQUE_ID)%>%
  filter(n()>1)

table(duplic$nrsa_survey)
#nrsa0809 nrsa1314 nrsa1819
#   1059     1462     1128

table(all_dat_id$nrsa_survey,all_dat_id$VISIT_NO)
#           1    2
#  nrsa0809 2123  180
#  nrsa1314 2069  192
#  nrsa1819 1919  189

nrsa0809_subset<- all_dat_id%>%
  filter(nrsa_survey=="nrsa0809")

nrsa1314_proc<- all_dat_id%>%
  filter(nrsa_survey=="nrsa1314")

nrsa1819_proc<- all_dat_id%>%
  filter(nrsa_survey=="nrsa1819")

siteid_0809<-nrsa0809_subset$UNIQUE_ID
siteid_1314<-nrsa1314_proc$UNIQUE_ID
siteid_1819<-nrsa1819_proc$UNIQUE_ID

# SUBSET NRSA1314 data by dropping observations with same UNIQUE_ID as NRSA0809
# n = 1283 out of 2069
nrsa1314_subset<-all_dat_id%>%
  filter(nrsa_survey=="nrsa1314")%>%
  filter(!UNIQUE_ID %in%siteid_0809)

# SUBSET NRSA1819 by dropping sites sampled in 0809 and/or 1314
# 1560 - after dropping NRSA0809 duplicates
# 992 obs - after dropping NRSA1314 duplicates
nrsa1819_subset <- all_dat_id %>%
  filter(nrsa_survey=="nrsa1819")%>%
  filter(!UNIQUE_ID %in% siteid_0809) %>%
  filter(!UNIQUE_ID %in% siteid_1314)

########################
## CREATE NEW COLUMN INDICATING WHETHER RESAMPLED SITE
# READ IN FULL NRSA COMPILED DATASET
#all_dat_id<-read.csv("data_processed/Compiled/NRSA_081318_all.csv")

# ONLY IN 2008-09 (n = 1420)
nrsa0809_only<-all_dat_id%>%
  filter(nrsa_survey=="nrsa0809")%>%
  filter(!UNIQUE_ID %in% siteid_1314) %>%
  filter(!UNIQUE_ID %in% siteid_1819)%>%
  mutate(RESAMPLE="NRSA0809")
#names(nrsa0809_only)

# ONLY IN 2013-14 n = 803
nrsa1314_only <- all_dat_id%>%
  filter(nrsa_survey=="nrsa1314")%>%
  filter(!UNIQUE_ID %in% siteid_0809)%>%
  filter(!UNIQUE_ID %in% siteid_1819)%>%
  mutate(RESAMPLE="NRSA1314")

# ONLY IN 2018-19 n=992
nrsa1819_only <- all_dat_id%>%
  filter(nrsa_survey=="nrsa1819")%>%
  filter(!UNIQUE_ID %in% siteid_0809)%>%
  filter(!UNIQUE_ID %in% siteid_1314)%>%
  mutate(RESAMPLE="NRSA1819")

# RESAMPLED sites from 0809 n=883 - filter 2008-09 for matching IDS in either 2013-14 or 2018-19
nrsa0809_resamp<- all_dat_id %>%
  filter(nrsa_survey=="nrsa0809")%>%
  filter(UNIQUE_ID %in% siteid_1314|UNIQUE_ID %in% siteid_1819)%>%
  mutate(RESAMPLE="RESAMPLED")

# RESAMPLED sites from 1314 n = 1458 OLD=480
nrsa1314_resamp<- all_dat_id %>%
  filter(nrsa_survey=="nrsa1314")%>%
  #filter(!UNIQUE_ID %in% siteid_0809) %>% # Not in 0809
  filter(UNIQUE_ID %in% siteid_0809|UNIQUE_ID %in% siteid_1819)%>% # does have matching ids with 2008-09 or 2018-19
  mutate(RESAMPLE="RESAMPLED")

# RESAMPLED sites from 1819 n = 1118
nrsa1819_resamp<- all_dat_id %>%
  filter(nrsa_survey=="nrsa1819")%>%
  filter(UNIQUE_ID %in% siteid_0809|UNIQUE_ID %in% siteid_1314)%>% # does have matching ids with 2008-09 or 2018-19
  mutate(RESAMPLE="RESAMPLED")

###################
## BRING DATASETS TOGETHER n = 6674
all_dat_re<-bind_rows(nrsa0809_only,nrsa0809_resamp,nrsa1314_only,nrsa1314_resamp,nrsa1819_only,nrsa1819_resamp)
table(all_dat_re$RESAMPLE)
#NRSA0809  NRSA1314  NRSA1819 RESAMPLED
# 1420       803       992      3459


######################
## MASTER LIST OF NRSA SITES AND RT_MASTER designation (Email from Darin 10/28/22)
#   n=8640 from 2000-2019
ref_nrsa<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_site_R_T/sample.info_12022021.csv")

ref_nrsa<-ref_nrsa%>%
  select(SITE_ID,YEAR,VISIT_NO,DOY,RT_MASTER)

### Merge with processed NRSA data n=4371
all_dat_resample<-left_join(all_dat_re,ref_nrsa,by=c("SITE_ID","VISIT_NO","YEAR"))
table(all_dat_resample$RT_MASTER)
# ?    B    R    S    T
# 35  156  869 3567 1983
table(all_dat_resample$RT_WQI)


###############
## PROCESS VARIABLES
###########
# FIX PTL_RESULT
# ONE OBS has PTL_RESULT = -2 (NRS18_OK_11082, VISIT_NO=1)
all_dat_resample<-all_dat_resample %>%
  mutate(PTL_RESULT = na_if(PTL_RESULT, -2))
summary(all_dat_resample$PTL_RESULT)

# MAKE d-excess AN EVAPORATION INDEX WHERE POSITIVE NUMBERS MEAN MORE EVAPORATION
# AND SCALE BY 10 to make variance on similar scale as other predictors
all_dat_resample<- all_dat_resample%>%
  mutate(d.excess_sc = d.excess/10)%>%
  mutate(evap_index=(d.excess - 10)*-1)%>%
  mutate(evap_index_sc = evap_index/10)

summary(all_dat_resample$d.excess)
summary(all_dat_resample$d.excess_sc)
summary(all_dat_resample$evap_index)
summary(all_dat_resample$evap_index_sc)
var(all_dat_resample$evap_index,na.rm = T) #[1] 43.90143
var(all_dat_resample$evap_index_sc,na.rm = T) # [1] 0.4390143


# CREATE CRITCIAL DIAMETER VARIABLE (Phil email 5/26/22)
#  Compared to the LDCBF_G08 and the derived var is a little different for some and fills in some NAs
all_dat_resample <- all_dat_resample%>%
  mutate(LDCBF_use = LSUB_DMM - LRBS_use)#%>%
  #select(LDCBF_use, LDCBF_G08)
summary(all_dat_resample$LDCBF_use)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#-3.4231  0.7004  1.1577  1.1199  1.6213  4.2537     193
summary(all_dat_resample$LDCBF_G08)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#-3.4231  0.6542  1.1209  1.0704  1.5707  4.2508     370

#############
# CREATE "Specific Stream Power" - suggestion by Phil (abstact comment 10/27/22)
#  Slope*depth = index of stream power
#  Slope*depth/width = Specific Stream Power - close to shear stress
all_dat_resample<- all_dat_resample%>%
  mutate(sp_strm_pwr = (XSLOPE_use+0.01)*(XDEPTH_CM/100)/(XWIDTH_use)) # convert depth (cm) to m
summary(all_dat_resample$sp_strm_pwr)

#####################
## TRANSFORM CHEMISTRY VARIABLES FOR MODEL
# one observation has PTL_RESULT = -2 (SITE_ID = NRS18_OK_11082)
all_dat_resample <- all_dat_resample%>%
  mutate(L_NO3 = log10(NITRATE_N_RESULT+0.001),
         L_NO2 = log10(NITRITE_N_RESULT+0.00001),
         L_PTL = log10(PTL_RESULT+0.01),
         L_NTL = log10(NTL_RESULT+0.01),
         L_CHLR = log10(CHLORIDE_RESULT),
         L_SULF = log10(SULFATE_RESULT),
         L_TSS = log10(TSS_RESULT+0.01),
         L_TURB = log10(TURB_RESULT),
         L_SODIUM = log10(SODIUM_RESULT))

#################
# SUM LAND USE CLASSES
all_dat_resample <-all_dat_resample%>%
  mutate(PCTAGR_WS = PCTHAY_WS+PCTCROP_WS,
         PCTURB_WS = PCTURBOP_WS+PCTURBLO_WS+PCTURBMD_WS+PCTURBHI_WS,
         PCTWET_WS = PCTHBWET_WS+PCTWDWET_WS,
         PCTFOR_WS = PCTMXFST_WS+PCTCONIF_WS+PCTDECID_WS,
         PCTAGR_WsRp100 = PCTHAY_WsRp100+PCTCROP_WsRp100,
         PCTURB_WsRp100 = PCTURBOP_WsRp100+PCTURBLO_WsRp100+PCTURBMD_WsRp100+PCTURBHI_WsRp100,
         PCTWET_WsRp100 = PCTHBWET_WsRp100+PCTWDWET_WsRp100,
         PCTFOR_WsRp100 = PCTMXFST_WsRp100+PCTCONIF_WsRp100+PCTDECID_WsRp100,
         PCTNATTERR_WsRp100 = PCTDECID_WsRp100 + PCTCONIF_WsRp100 + PCTMXFST_WsRp100 + PCTSHRB_WsRp100 + PCTGRS_WsRp100,
         PCTNAT_WsRp100 = PCTNATTERR_WsRp100 + PCTWET_WsRp100)
summary(all_dat_resample$PCTURB_WsRp100)
summary(all_dat_resample$PCTWET_WsRp100)


####################
# CONVERT NABD_STORAGE FROM m3/sqkm to m3/m2
# CONVERT RunoffWs FROM mm to m
# CALCULATE DAM STORAGE RELATIVE TO WATERSHED RUNOFF - ratio (unitless)
all_dat_resample <- all_dat_resample %>%
  mutate(NABD_NIDStorWs_m = NABD_NIDStorWs/1000000,
         NABD_NrmStorWs_m = NABD_NrmStorWs/1000000,
         RunoffWs_m = RunoffWs*0.001,
         NABD_NIDStorWs_ratio = NABD_NIDStorWs_m/RunoffWs_m,
         NABD_NrmStorWs_ratio = NABD_NrmStorWs_m/RunoffWs_m)

# TRANSFORM LANDUSE CLASSES - Added natural riparian cover classes (terrestrial and all (forest, shrub, grass, wetland))
all_dat_resample <- all_dat_resample%>%
  mutate(L_NABD_NrmStorWs_ratio = log10(NABD_NrmStorWs_ratio+0.001),
         asin_PCTAGR_WS = asin(sqrt(PCTAGR_WS/100)),
         asin_PCTURB_WS = asin(sqrt(PCTURB_WS/100)),
         asin_PCTWET_WS = asin(sqrt(PCTWDWET_WS/100)),
         asin_PCTFOR_WS = asin(sqrt(PCTFOR_WS/100)),
         PCTFOR_WsRp100_mod = floor(PCTFOR_WsRp100),
         asin_PCTFOR_WsRp100 = asin(sqrt(PCTFOR_WsRp100_mod/100)),
         asin_PCTWET_WsRp100 = asin(sqrt(PCTWET_WsRp100/100)),
         asin_PCTAGR_WsRp100 = asin(sqrt(PCTAGR_WsRp100/100)),
         PCTURB_WsRp100_mod = floor(PCTURB_WsRp100),
         asin_PCTURB_WsRp100 = asin(sqrt(PCTURB_WsRp100_mod/100)),
         PCTNATTERR_WsRp100_mod=floor(PCTNATTERR_WsRp100),
         asin_PCTNATTERR_WsRp100 = asin(sqrt(PCTNATTERR_WsRp100_mod/100)),
         PCTNAT_WsRp100_mod = floor(PCTNAT_WsRp100),
         asin_PCTNAT_WsRp100 = asin(sqrt(PCTNAT_WsRp100_mod/100)))

# STREAM MORPH TRANSFORMATIONS
all_dat_resample <-all_dat_resample %>%
  mutate(L_XWIDTH_use = log10(XWIDTH_use),
         L_XWXD = log10(XWXD+0.01),
         L_SLOPE = log10(XSLOPE_use+0.01),
         L_SLOPE_DPTH = log10((XSLOPE_use+0.01)*XDEPTH_CM),
         L_STRM_POWER = log10(sp_strm_pwr))
summary(all_dat_resample$L_SLOPE_DPTH)
summary(all_dat_resample$L_STRM_POWER)

# TRANSFORM RD DENSITY
all_dat_resample<-all_dat_resample%>%
  mutate(L_RdDensWs=log10(RdDensWs+0.1),
         L_RdDensWsRp100=log10(RdDensWsRp100+0.1))
summary(all_dat_resample$L_RdDensWs)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#-1.000000  0.003969  0.171966  0.138623  0.295188  1.183338

#################
# 5/4/2022
# READ IN PROCESSED DATA - COMPILED THREE SURVEYS, VISITS 1 & 2 n=6674
#dat<- read.csv("data_processed/Compiled/NRSA_081318_all.csv")

# MMI and EPT Alan Emailed 4/25/22
mmi <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_benthic_indices/NRSA_BUGS_2022_0425.csv")
names(mmi)
#  "SITE_ID"   "YEAR" "VISIT_NO"  "EPT_RICH"  "UNIQUE_ID" "MMI_BENT"
# REDUCE MMI data to just unique ID and indices
mmi_red <- mmi %>%
  select(SITE_ID,VISIT_NO,YEAR, EPT_RICH, MMI_BENT)

#############
# MERGE DATA TOGETHER
# Merge processed NRSA and StreamCat datasets together using left_join n=6674
all_dat_resample2<-left_join(all_dat_resample,mmi_red,by=c("SITE_ID","VISIT_NO","YEAR"))


####################
## SCALE MMI 0-10
all_dat_resample2$MMI_BENT_sc <- all_dat_resample2$MMI_BENT/100
summary(all_dat_resample2$MMI_BENT_sc)
var(all_dat_resample2$MMI_BENT_sc,na.rm = T) #[1] 0.04159292
#var(all_dat_resample2$MMI_BENT,na.rm = T) #[1] 415.9292
#var(all_dat_resample2$LRBS_use,na.rm=T) #[1] 1.378704
#var(all_dat_resample2$asin_PCTAGR_WS) #[1] 0.1297567
#var(all_dat_resample2$LQLow_kmcl,na.rm=T) #[1] 1.019986

## SCALE EPT 0-3.5
all_dat_resample2$EPT_RICH_sc <- all_dat_resample2$EPT_RICH/10
summary(all_dat_resample2$EPT_RICH_sc)
#summary(all_dat_resample2$EPT_RICH)
var(all_dat_resample2$EPT_RICH_sc,na.rm = T) #[1] 0.3785358
#var(all_dat_resample2$EPT_RICH,na.rm = T) #[1] 37.85358

# OE
#var(all_dat_resample2$OE_SCORE,na.rm=T) #[1] 0.09672672

#####################
## READ IN PROCESSED PHDI data 5/26/22 n = 6674, 4 vars
#   Calculated survey year mean PHDI and PHDI for month sample was collected
phdi<-read.csv("data_processed/Compiled/phdi_mean_month.csv")

# MERGE WITH PROCESSED NRSA DATA n = 6674
all_dat_resample3 <-left_join(all_dat_resample2, phdi,
                          by=c("UNIQUE_ID","DATE_COL"))

names(all_dat_resample3)

## CREATE PHDI INDEX INDICATING DROUGHT WHERE POSITIVE VALUES MEAN MORE DROUGHT AND NEGATIVE MEAN WET
all_dat_resample3<- all_dat_resample3%>%
  mutate(drought_mean=phdi_mean*-1)%>%
  mutate(drought_month=phdi_month*-1)

summary(all_dat_resample3$drought_mean)
var(all_dat_resample3$drought_mean,na.rm = T) #[1] 5.779492

###################
## CREATE NEW ECOREGION BASED ON EMAP-WEST
# SEE ALAN H Email 6/27/22
all_dat_resample3<- all_dat_resample3 %>%
  mutate(ECO_L3_mod = case_when(
    (US_L3CODE %in% c('23','8'))~'MT-SWEST',
    (US_L3CODE %in% c('11','15','16','17','41'))~'MT-NROCK',
    (US_L3CODE %in% c('1','4','5','77','78','9'))~'MT-PNW',
    (US_L3CODE %in% c('19','21'))~'MT-SROCK',
    (US_L3CODE %in% c('25','46','47','48'))~'PL-NCULT',
    (US_L3CODE %in% c('26','42','43','44'))~'PL-RANGE',
    (US_L3CODE %in% c('10','12','80'))~'XE-NORTH',
    (US_L3CODE %in% c('6','7'))~'XE-CALIF',
    (US_L3CODE %in% c('18','20','22'))~'XE-EPLAT',
    (US_L3CODE %in% c('13','14','79','81','24'))~'XE-SOUTH',
  ))
table(all_dat_resample3$ECO_L3_mod)
#MT-NROCK   MT-PNW MT-SROCK MT-SWEST PL-NCULT PL-RANGE XE-CALIF XE-EPLAT XE-NORTH XE-SOUTH
#   327      245      165       44      383      657       69      249      161      183

# Met with Alan and Phil 6/28/22
# Decided to start by lumping Western mountains together (N. Rockies + Pacific NW mount)
# And Appalachian
all_dat_resample3<- all_dat_resample3 %>%
  mutate(ECO_L3_4_mod = case_when(
    (US_L3CODE %in% c('23','8'))~'MT-SWEST',
    (US_L3CODE %in% c('11','15','16','17','41','1','4','5','77','78','9'))~'MT-WEST',
    (US_L3CODE %in% c('19','21'))~'MT-SROCK',
    (US_L3CODE %in% c('25','46','47','48'))~'PL-NCULT',
    (US_L3CODE %in% c('26','42','43','44'))~'PL-RANGE',
    (US_L3CODE %in% c('10','12','80'))~'XE-NORTH',
    (US_L3CODE %in% c('6','7'))~'XE-CALIF',
    (US_L3CODE %in% c('18','20','22'))~'XE-EPLAT',
    (US_L3CODE %in% c('13','14','79','81','24'))~'XE-SOUTH',
    (US_L4CODE %in% c('67a','67b','67f','67g'))~'APP-valley',# Identify whether App ridge or valley
    (US_L4CODE %in% c('67c','67d','67h','67i'))~'APP-ridge',
    (US_L3CODE %in% c('58','60','62','66','68','69'))~'APP-ridge'
  ))

table(all_dat_resample3$ECO_L3_4_mod)
#APP-ridge APP-valley   MT-SROCK   MT-SWEST    MT-WEST   PL-NCULT   PL-RANGE   XE-CALIF
#596        109        165         44        572        383        657         69
#XE-EPLAT   XE-NORTH   XE-SOUTH
#249        161        183

######################
## ADD INDIVIDUAL O and E values 9/6/2022 and 11/2/2022
## NRSA CROSSWALK UID
uid<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/O_E_components/NRSA_UID_Crosswalk.csv")

# DATASETS WITH O and E components - Need to crosswalk old UID with updated UIDs for the 0809 and 1314 datasets
## NRSA O & E 0809
#    note there are many observations with old UIDs that start with 500 that are not part of the NRSA survey but have bug information
oe_0809<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/O_E_components/all.sites.OE.scores.cal-val-test.csv")
# Rename UID column to indicate old version
oe_0809 <- oe_0809%>%
  rename("old_UID"="UID")

## NRSA O & E 1314
oe_1314<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/O_E_components/new.sites.OE.scores.csv")
# Rename first column to be UID
oe_1314<-oe_1314%>%
  rename("old_UID"="X")
names(oe_1314)

##################
## PROCESS O and E 0809 and 1314 data - MERGE with crosswalk dataset - "old_UID" = "SOURCE_ID"
oe_0809_proc<-left_join(oe_0809, uid, by=c("old_UID"="SOURCE_ID")) #n = 4190 from 3794 bc there are several observations without updated UID
summary(oe_0809_proc$O)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.00    7.00   10.00   10.61   14.00   30.00
summary(oe_0809_proc$DESTINATION_ID)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   1317   12269   14332  182936   15475 1003381    1704
table(oe_0809_proc$SOURCE_STUDYNAME)
#NRSA0809 NRSA1314
#2060      426

# RENAME DESTINATION ID to UID
oe_0809_proc<- oe_0809_proc%>%
  rename("UID"="DESTINATION_ID")%>%
  select(!c(X, GROUP, ECO3,nUID,old_UID,SOURCE_STUDYNAME,DESTINATION_STUDYNAME))
names(oe_0809_proc)

# Join 1314 O & E with crosswalk table
oe_1314_proc<-left_join(oe_1314,uid,by=c("old_UID"="SOURCE_ID")) #n = 2667 from 2254
summary(oe_1314_proc$O)
summary(oe_1314_proc$DESTINATION_ID)
table(oe_1314_proc$SOURCE_STUDYNAME)
#NRSA0809 NRSA1314 NRSA1819
#336     2254       77
length(unique(oe_1314_proc$DESTINATION_ID)) #n = 2667

# RENAME DESTINATION ID to UID
oe_1314_proc<- oe_1314_proc%>%
  rename("UID"="DESTINATION_ID")%>%
  select(!c(old_UID,SOURCE_STUDYNAME,DESTINATION_STUDYNAME))
names(oe_1314_proc)

##############
## NRSA O & E 1819 - don't need to update UID bc Karen did
oe_1819<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/O_E_components/NRSA1819_OE_scores_updUIDs.csv")


################
## SUBSET PROCESSED NRSA DATASET BY SURVEY YEAR TO MERGE INDIVIDUALLY WITH data with O & E
nrsa0809<- all_dat_resample3%>%
  filter(nrsa_survey=="nrsa0809")
nrsa1314<-all_dat_resample3%>%
  filter(nrsa_survey=="nrsa1314")
nrsa1819<-all_dat_resample3%>%
  filter(nrsa_survey=="nrsa1819")

# MERGE processed data with O and E based on UID
# NRSA 0809 n = 22303
nrsa_oe_0809<-left_join(nrsa0809,oe_0809_proc, by=c("UID"="UID"))#n=97671
length(unique(nrsa_oe_0809$UID))# 2248
#head(nrsa_oe_0809)
summary(nrsa_oe_0809$E)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#4.267  10.157  12.932  13.012  15.045  25.793     199
# Not sure why so many duplicates
dup0809<-nrsa_oe_0809%>%
  filter(duplicated(UID)) #n=95423

# Drop duplicate obs n = 2303 based on SITE_ID and VISIT_NO
nrsa_oe_0809<-nrsa_oe_0809%>%
  distinct(SITE_ID,VISIT_NO,YEAR, .keep_all = T)

############
# NRSA 1314 n = 2261
nrsa_oe_1314<- left_join(nrsa1314, oe_1314_proc, by=c("UID"="UID")) #n=2261
length(unique(nrsa_oe_1314$UID))#n=2256
table(nrsa_oe_1314$SOURCE_STUDYNAME)

################
## NRSA 1819 n=2110
nrsa_oe_1819<- left_join(nrsa1819, oe_1819, by=c("UID"="UID")) #n= 2110

## COMPILE THREE SURVEYS TOGETHER n=6674
nrsa_oe_all<-bind_rows(nrsa_oe_0809,nrsa_oe_1314,nrsa_oe_1819)


#################
## EXPORT CSV OF COMPILED DATA - ALL OBSERVATIONS, ALL SURVEYS VISITS 1 & 2
write.csv(nrsa_oe_all,"data_processed/Compiled/NRSA_081318_all_O_E.csv", row.names=FALSE)

#COLUMN NAMES
dat_names<-data.frame(colnames(nrsa_oe_all))
write.csv(dat_names,"data_processed/Compiled/column_vars_O_E.csv",row.names = FALSE)



####################
# CREATE DATASET OF SINGLE SITE ACROSS THREE SURVEYS
#  version where retain all early surveys and remove resamples in later surveys
# ALL OF 0809 and not resampled sites in subsequent surveys
#  INCLUDES VISIT 1 & 2

# NRSA0809 n = 2303
nrsa0809_subset<- nrsa_oe_all%>%
  filter(nrsa_survey=="nrsa0809")

# SUBSET NRSA1314 data by dropping observations with same UNIQUE_ID as NRSA0809
# n = 1283 out of 2069
nrsa1314_subset<-nrsa_oe_all%>%
  filter(nrsa_survey=="nrsa1314")%>%
  filter(!UNIQUE_ID %in%siteid_0809)

# SUBSET NRSA1819 by dropping sites sampled in 0809 and/or 1314
# 1560 - after dropping NRSA0809 duplicates
# 992 obs - after dropping NRSA1314 duplicates
nrsa1819_subset <- nrsa_oe_all%>%
  filter(nrsa_survey=="nrsa1819")%>%
  filter(!UNIQUE_ID %in% siteid_0809) %>%
  filter(!UNIQUE_ID %in% siteid_1314)

###################
## BRING SUBSETS TOGETHER
nrsa_all<-bind_rows(nrsa0809_subset,nrsa1314_subset,nrsa1819_subset)
# n = 4578 w/262 vars

table(nrsa_all$nrsa_survey,nrsa_all$VISIT_NO)
#              1    2
#nrsa0809   2123  180
#nrsa1314   1279    4
#nrsa1819    987    5

##################
## SELECT VISIT_NO=1
nrsa_all_visit1 <- nrsa_all%>%
  filter(VISIT_NO == 1)
#n = 4389 obs/169 variables
table(nrsa_all_visit1$VISIT_NO)
length(unique(nrsa_all_visit1$SITE_ID))
length(unique(nrsa_all_visit1$UNIQUE_ID))

table(nrsa_all_visit1$nrsa_survey)
#nrsa0809 nrsa1314 nrsa1819
#2123     1279      987

table(nrsa_all_visit1$nrsa_survey,nrsa_all_visit1$YEAR)
#         2008 2009 2013 2014 2018 2019
#nrsa0809  791 1332    0    0    0    0
#nrsa1314    0    0  525  754    0    0
#nrsa1819    0    0    0    0  449  538

#########################
## WRITE DATASETS

# NRSA 08-19 All 0809 sites and only new sites from subsequent surveys
#   INCLUDES VISIT 1 & 2 n = 4578
write.csv(nrsa_all,"data_processed/Compiled/nrsa081318_nonresampled_VISIT_12.csv",row.names = FALSE)


# NRSA 08-19 All 0809 sites and only new sites from subsequent surveys
#   ONLY VISIT 1 n = 4389
write.csv(nrsa_all_visit1,"data_processed/Compiled/nrsa081318_nonresampled_VISIT_1_ONLY.csv",row.names = FALSE)



##############
## FOR MAP - SELECT DISTINCT UNIQUE_IDS n = 4390
all_dat_unique= nrsa_all%>%
  distinct(UNIQUE_ID, .keep_all=TRUE)

# EXPORT csv for map
write.csv(all_dat_unique,"data_processed/Compiled/nrsa081318_for_map.csv", row.names = FALSE)









#####################
## TABLE OF UNIQUE SITES
table(all_dat_id$YEAR,all_dat_id$VISIT_NO)
#       1    2
#2008  791   69
#2009 1332  111
#2013  931   87
#2014 1138  105
#2018  941  100
#2019  978   89

unique0809<-all_dat_id %>%
  filter(nrsa_survey=="nrsa0809")%>%
  filter(!UNIQUE_ID%in%siteid_1314)%>%
  filter(!UNIQUE_ID%in%siteid_1819) %>%
  filter(VISIT_NO==1)
table(unique0809$YEAR)
#2008 2009
# 481  851
# There are 881 sites in 1314 that were originally sampled in 0809

unique1314<-all_dat_id %>%
  filter(nrsa_survey=="nrsa1314")%>%
  #filter(!UNIQUE_ID%in%siteid_0809)%>%
  filter(!UNIQUE_ID%in%siteid_1819) %>%
  filter(VISIT_NO==1)
table(unique1314$YEAR)
# 2013 2014
#  491  647
#  322  479

unique1819<-all_dat_id %>%
  filter(nrsa_survey=="nrsa1819")%>%
  #filter(!UNIQUE_ID%in%siteid_0809)%>%
  #filter(!UNIQUE_ID%in%siteid_1314) %>%
  filter(VISIT_NO==1)
table(unique1819$YEAR)
#2018 2019
#941  978
#2018 2019
#449  538

# CHECK
#test <-nrsa1314_proc[!nrsa1314_proc$UNIQUE_ID %in% nrsa0809_proc$UNIQUE_ID, c(1:168)]
length(unique(nrsa0809_proc$UNIQUE_ID))
length(unique(nrsa1314_proc$UNIQUE_ID))
both<-all_dat_id%>%
  filter(nrsa_survey=="nrsa0809"|nrsa_survey=="nrsa1314")
length(unique(both$UNIQUE_ID))
1283+2123
# WHEN select unique IDS for combined datasets 0809+1314 = 3402
# BUT When we add the unique 1314 (that does not include 0809 IDs) + 0809 unique IDs
#   we get = 3406 - there are four extra??






#############
## CHECK DISTRIBUTIONS ACROSS SURVEYS
dat_red<-all_dat%>%
  select(c('nrsa_survey',"AMMONIA_N_RESULT","ANC_RESULT","CHLORIDE_RESULT",
           "COLOR_RESULT","COND_RESULT","DOC_RESULT","MAGNESIUM_RESULT",
           "SODIUM_RESULT","POTASSIUM_RESULT","NTL_RESULT","PTL_RESULT",
           "SULFATE_RESULT","TSS_RESULT","TURB_RESULT"))
long_data <- melt(dat_red, id.vars = "nrsa_survey", variable.name="Element")

histograms <- ggplot(long_data, aes(log(value),fill=nrsa_survey))+
  geom_histogram()+
  facet_grid(vars(Element), vars(nrsa_survey))
#  facet_wrap(~Element)

#############
## HABITAT
hab_red<-all_dat%>%
  select(c('nrsa_survey',"LOE_RBS_use","LOE_XCMGW_use","LOE_XFC_NAT_use",
           "LOE_QLow_cl","LOE_Qbkf_cl"))#,"d.excess","WQII"))
long_hab <- melt(hab_red, id.vars = "nrsa_survey", variable.name="Element")

histogram_hab <- ggplot(long_hab, aes(log(value),fill=nrsa_survey))+
  geom_histogram()+
  facet_grid(vars(Element), vars(nrsa_survey))
#  facet_wrap(~Element)
histogram_hab

#############
## CLIMATE TEMP
clim_red<-all_dat%>%
  select(c('nrsa_survey',"MAST_SY","MSST_SY","MWST_SY",
           "TMEAN_WSY_WS","TMEAN_SSY_WS","TMEAN_SSY_WS","TMEAN_SY_WS",
           "Tmean8110Ws"))#,"d.excess","WQII"))
long_clim <- melt(clim_red, id.vars = "nrsa_survey", variable.name="Element")

histogram_clim <- ggplot(long_clim, aes(log(value),fill=nrsa_survey))+
  geom_histogram()+
  facet_grid(vars(Element), vars(nrsa_survey))
#  facet_wrap(~Element)
histogram_clim

##############
## ANTHROPOGENIC
human_red<-all_dat%>%
  select(c('nrsa_survey',"W1_HALL","W1H_WALL","W1_HAG",
           "W1H_CROP","W1_HNOAG"))#,"d.excess","WQII"))
long_human <- melt(human_red, id.vars = "nrsa_survey", variable.name="Element")

histogram_human <- ggplot(long_human, aes(log(value),fill=nrsa_survey))+
  geom_histogram()+
  facet_grid(vars(Element), vars(nrsa_survey))
#  facet_wrap(~Element)
histogram_human

LU_red<-all_dat%>%
  select(c('nrsa_survey',"PCTHAY_WS","PCTCROP_WS","PCTURBOP_WS",
           "PCTURBLO_WS","PCTURBMD_WS","PCTURBHI_WS",
           "PCTWDWET_WS","PCTHBWET_WS"))#,"d.excess","WQII"))
long_LU <- melt(LU_red, id.vars = "nrsa_survey", variable.name="Element")

histogram_LU <- ggplot(long_LU, aes(value,fill=nrsa_survey))+
  geom_histogram()+
  facet_grid(vars(Element), vars(nrsa_survey))

histogram_LU

##############
## OTHER
histogram_dexcess<-all_dat %>%
  filter(!is.na(d.excess))%>%
  ggplot(aes(x=d.excess, fill=nrsa_survey))+
  geom_histogram()+
  facet_grid(~nrsa_survey~.)

histogram_wqii<-all_dat %>%
  filter(!is.na(WQII))%>%
  ggplot(aes(x=WQII, fill=nrsa_survey))+
  geom_histogram()+
  facet_grid(nrsa_survey~.)

#############
## SAVE PLOTS
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Compiled_dat/Histogram_chem.tiff",
     width=7.5, height=10, units="in", res=200)
histograms
dev.off()

tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Compiled_dat/Histogram_hab.tiff",
     width=7.5, height=7, units="in", res=200)
histogram_hab
dev.off()

tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Compiled_dat/Histogram_wqii_dexcess.tiff",
     width=5, height=7, units="in", res=200)
grid.arrange(histogram_dexcess,histogram_wqii)
dev.off()




#############
## EXAMINE WATER QUALITY INDEX VALUES AND O/E for the 2013-14 data
# 2013-14 to start
wqii13<-wqii%>%
  filter(YEAR==2013|YEAR==2014)

# Merge with NRSA 2013-14
# Merge processed NRSA and StreamCat datasets with WQII using left_join
dat<-left_join(nrsa,wqii13,
                        by=c("SITE_ID","VISIT_NO"))

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

####################
## BIPLOTS
# WQII and benthic OE
benthic<-ggplot(dat, aes(x=WQII, y=OE_SCORE))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  #scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  facet_wrap(~AG_ECO5, ncol=5)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(family="RMN", label = ..r.label..), label.x=-4, size=3)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        #text = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("WQII"), #Grand mean scaled
       y=expression("Benthic O/E"))#

#Export
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/Biplot_bugOE_wqii.tiff",
     width=7.5, height=4, units="in", res=200)
benthic
dev.off()
