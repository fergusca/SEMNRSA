################
## LOAD NRSA DATA
###########
rm(list=ls())

# LOAD DATA - NRSA 2013-14 from website

# Libraries
library(dplyr)
library(tidyr)

library(devtools)
devtools::load_all
library(SEMNRSA)

# LOAD data stored in Rpackage - a .rda file
#load(file="data/nrsa.rda")

# ORIGINAL NRSA 2013-14 DATA downloaded from website 8/16/21
#siteinformation_wide,nrsa1314_widechem,bentmmi, landmet, phabmed

# On OneDrive
site_org <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_siteinformation_wide_04292019.csv")
chem_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_widechem_04232019.csv")
bent_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_bentmmi_04232019.csv")
land_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_landmet_02132019.csv")
phab_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_phabmed_04232019.csv")
strcat_org <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/StreamCat/NRSA_2013_14/FINAL_TABLE_NRSA_1314.csv")

# Load 2013-14 other NRSA data
benthic_oe_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_benthic_indices/NRSA2008-2019_OE_Scores.csv")
isotope_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/Water_isotope/NSRA 1314 H2O Isotopes.csv")
phab_oe_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_PHab_Discharge/NRSA_PHab_Discharge_OE891314.csv")
wqii_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/WQII/wqii_nrsa.csv")

# FORMAT DATE IN SITE
site_org$DATE_COL<-as.Date(site_org$DATE_COL, format="%m/%d/%Y")
str(site_org$DATE_COL)
site_org$YEAR <-format(site_org$DATE_COL,"%Y")
table(site_org$YEAR)
# 2013 2014
# 1018 1243

# Reduce datasets to merge together
chem_vars <- c("SITE_ID","VISIT_NO",
               "AMMONIA_N_RESULT",'AMMONIA_N_RESULT_UNITS',
               "ANC_RESULT","ANC_RESULT_UNITS",
               "CALCIUM_RESULT","CALCIUM_RESULT_UNITS",
               "CHLORIDE_RESULT","CHLORIDE_RESULT_UNITS",
               "COLOR_RESULT", "COLOR_RESULT_UNITS",
               "COND_RESULT","COND_RESULT_UNITS",
               "DOC_RESULT","DOC_RESULT_UNITS",
               "MAGNESIUM_RESULT","MAGNESIUM_RESULT_UNITS",
               "NITRATE_N_RESULT","NITRATE_N_RESULT_UNITS",
               "NITRATE_NITRITE_N_RESULT","NITRATE_NITRITE_N_RESULT_UNITS",
               "NITRITE_N_RESULT","NITRITE_N_RESULT_UNITS",
               "NTL_RESULT", "NTL_RESULT_UNITS",
               "PH_RESULT","PH_RESULT_UNITS",
               "POTASSIUM_RESULT","POTASSIUM_RESULT_UNITS",
               "PTL_RESULT","PTL_RESULT_UNITS",
               "SILICA_RESULT","SILICA_RESULT_UNITS",
               "SODIUM_RESULT", "SODIUM_RESULT_UNITS",
               "SULFATE_RESULT","SULFATE_RESULT_UNITS",
               "TSS_RESULT","TSS_RESULT_UNITS",
               "TURB_RESULT", "TURB_RESULT_UNITS")
chem <-chem_org[chem_vars]

bent<-bent_org%>%
  rename("OE_SCORE_OLD"="OE_SCORE")%>%
  select(c("SITE_ID","VISIT_NO","MMI_BENT","OE_SCORE_OLD"))

land<-land_org%>%
  select(-c("PUBLICATION_DATE","UID","DATE_COL",
            "SITETYPE","INDEX_VISIT",'LAT_DD83',"LON_DD83"))

phab <-phab_org%>%
  select(c("SITE_ID","VISIT_NO","PROTOCOL","XDEPTH_CM","SDDEPTH_CM",'XWIDTH',"XWXD","RP100","RPGT50","XBKF_W","XBKF_H","XINC_H","SINU","XSLOPE","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SA","PCT_SAFN","PCT_SFGF",
           "LDCBF_G08","LDMB_BW5","LRBS_BW5","LRBS_G08",
           "RB3","REYP3","RRPW3",
           "PCT_FAST","PCT_POOL","PCT_SLOW",
           "RPMXDEP_CM","RPRAT","RPXDEP_CM",
           "PCT_BDRK",
           "XFC_ALG","XFC_AQM","XFC_LWD","XFC_BRS", "XFC_OHV","XFC_BIG","XFC_NAT","XFC_UCB","V1W_MSQ",
           "XCDENBK","XCDENMID","XCL","XGB","XC","XCM","XCMGW","XPCAN","XPCM","XPCMG",
           "QR1","QRVEG1","RDIST1",
           "W1_HALL","W1H_WALL","W1H_LOG","W1_HNOAG","W1_HAG"))

strcat <-strcat_org%>%
  select("SITE_ID",
         "CatAreaSqKm","WsAreaSqKm","CatAreaSqKmRp100","WsAreaSqKmRp100",
         "HydrlCondWs","OMHWs",
         "DamDensWs","DamNIDStorWs","DamNrmStorWs",
         "NABD_DensWs","NABD_NIDStorWs","NABD_NrmStorWs",
         "MineDensWs","MineDensWsRp100",
         "AgKffactWs","FertWs","ManureWs","NPDESDensWs","NPDESDensWsRp100",
         "SuperfundDensWs",
         "HUDen2010Ws","HUDen2010WsRp100","PopDen2010Ws","PopDen2010WsRp100",
         "PctOw2013Ws","PctIce2013Ws","PctUrbOp2013Ws","PctUrbLo2013Ws","PctUrbMd2013Ws","PctUrbHi2013Ws",
         "PctDecid2013Ws","PctConif2013Ws","PctMxFst2013Ws",
         "PctShrb2013Ws","PctGrs2013Ws",
         "PctHay2013Ws","PctCrop2013Ws",
         "PctWdWet2013Ws","PctHbWet2013Ws",
         "PctOw2013_WsRp100","PctIce2013_WsRp100","PctUrbOp2013_WsRp100","PctUrbLo2013_WsRp100","PctUrbMd2013_WsRp100","PctUrbHi2013_WsRp100",
         "PctDecid2013_WsRp100","PctConif2013_WsRp100","PctMxFst2013_WsRp100",
         "PctShrb2013_WsRp100","PctGrs2013_WsRp100",
         "PctHay2013_WsRp100","PctCrop2013_WsRp100",
         "PctWdWet2013_WsRp100","PctHbWet2013_WsRp100",
         "PctFrstLoss2013Ws","PctFrstLoss2013WsRp100",
         "PctImp2013Ws","PctImp2013_RipBuf100WsRp100",
         "BFIWs","BFI_WS_PctFull",
         "PctNOT_PRINCIPLEWs","PctSANDSTONEWs","PctSEMICONSOL_SANDWs","PctVOLCANICWs",
         "PctSANDSTONE_CARBONATEWs","PctUNCONSOL_SAND_GRAVELWs","PctCARBONATEWs",
         "ClayWs",'PctCarbResidWs',"PctNonCarbResidWs","PctAlkIntruVolWs","PctSilicicWs",
         "PctExtruVolWs","PctColluvSedWs",
         "PctGlacTilClayWs","PctGlacTilLoamWs","PctGlacTilCrsWs","PctGlacLakeCrsWs","PctGlacLakeFineWs",
         "PctHydricWs","PctEolCrsWs","PctEolFineWs","PctSalLakeWs","PctAlluvCoastWs","PctCoastCrsWs","PctWaterWs",
         "ElevWs","PermWs",
         "KffactWs","Kffact_PT","LSTFRZWs","LSTFRZ_PT","RDHWs","RDH_PT","RHMEANWs","RHMEAN_PT",
         "RunoffWs","WDMAX_PT","WDMIN_PT","WDSUM_PT",
         "PMAXWs","PMINWs","Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws",
         "Precip08Ws","Precip09Ws",
         "TMEAN_S_2013Ws","TMEAN_S_2013_PT","TMEAN_S_2014Ws","TMEAN_S_2014_PT","TMEAN_W_2013Ws","TMEAN_W_2013_PT","TMEAN_W_2014Ws","TMEAN_W_2014_PT","TMEAN_PW_2013Ws","TMEAN_PW_2013_PT","TMEAN_PW_2014Ws","TMEAN_PW_2014_PT",
         "TMEAN_SY_2013Ws","TMEAN_SY_2013_PT","TMEAN_SY_2014Ws","TMEAN_SY_2014_PT",
         "PSUMPY_2013Ws","PSUMPY_2013_PT","PSUMPY_2014Ws","PSUMPY_2014_PT",
         "MAST_2013","MAST_2014","MSST_2013","MSST_2014","MWST_2013","MWST_2014")

# Updated benthic O/E
# PROCESS DATA - SELECT YEAR 2013-14
# DATE_COL from a character to a date
benthic_oe_org$DATE_COL<-as.Date(benthic_oe_org$DATE_COL, format="%m/%d/%Y")
benthic_oe_org$YEAR <-format(benthic_oe_org$DATE_COL,"%Y")
benthic_oe <-benthic_oe_org%>%
  filter(YEAR==2013|YEAR==2014)%>%
  select(c("SITE_ID","VISIT_NO","OE_SCORE"))

#PHab O/E
phab_oe <- phab_oe_org%>%
  filter(YEAR==2013|YEAR==2014)%>%
  select(c("SITE_ID","VISIT_NO","REALM","W1H_CROP",
           "XSLOPE_use","XWIDTH_use","Lpt01_XCMGW","Lpt01_XFC_NAT",
           "LRBS_use","RDIST_COND","LRBS_Cond_use","LOE_RBS_use",
           "LXCMGW_Cond_use","LOE_XCMGW_use","LXFC_NAT_Cond_use","LOE_XFC_NAT_use",
           "Qbkf_cl","LQbkf_cl","Qbkf_kmcl","LQbkf_kmcl",
           "QLow_cl","LQLow_cl","QLow_kmcl","LQLow_kmcl",
           "LOE_QLow_cl","LOE_Qbkf_cl"
           ))

length(unique(phab_oe$SITE_ID))#n=2081

# Check for duplicate obs n = 3 SITE_ID= MORM-1002
test<-phab_oe%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)

# WQII
names(wqii_org)
wqii<- wqii_org%>%
  filter(YEAR==2013|YEAR==2014)%>%
  select(c("SITE_ID","VISIT_NO","RT_WQI",
           "CL_pt","SO4_pt","PTL_pt","NTL_pt",
           "TURB_pt","ENTERO_PT",
           "DO_PT","PH_PT","NUMVARN","WQII"))

length(unique(wqii$SITE_ID)) #n=2070


# Water isotopes
isotope_org$DATE_COL<-as.Date(isotope_org$DATE_COL, format="%m/%d/%Y")
isotope_org$YEAR <-format(isotope_org$DATE_COL,"%Y")
table(isotope_org$YEAR)
#2013 2014
#1046 1279
isotope <- isotope_org%>%
  select(c("SITE_ID","VISIT_NO","SAMPLE_ID","Chem_Lab_ID","H2O_dD","H2O_d18O","d.excess"))
length(unique(isotope$SITE_ID))#2129

# Check for duplicates n=8 (2 each but have different isotope values)
# OKLS-1176,WILS-1089,WIS9-0935,FLS9-0921
test<-isotope%>%
  group_by(SITE_ID)%>%
  filter(n()>1)

# WRITE REDUCED DATASETS TO NEW FOLDER TO MERGE
write.csv(site_org,"data_to_merge/a_site.csv", row.names=FALSE)
write.csv(bent,"data_to_merge/b_benth.csv", row.names=FALSE)
write.csv(benthic_oe,"data_to_merge/c_benth_oe.csv", row.names=FALSE)
write.csv(chem,"data_to_merge/c_chem.csv", row.names=FALSE)
#write.csv(land,"data_to_merge/d_land.csv", row.names=FALSE)
write.csv(phab,"data_to_merge/e_phab.csv", row.names=FALSE)

write.csv(phab_oe,"data_to_merge/f_phab_oe.csv", row.names=FALSE)
write.csv(wqii,"data_to_merge/g_wqii.csv", row.names=FALSE)
write.csv(isotope,"data_to_merge/h_isotope.csv", row.names=FALSE)


# Save in processed data folder
write.csv(strcat,"data_processed/subset_streamcat1314.csv", row.names=FALSE)

#########
## Use multi-merge function to bring data together
# Merging based on Site ID

# Need to treat SEMNRSA like a package to be able to run multimerge function
devtools::load_all()

nrsa<- multimerge("data_to_merge")
names(nrsa)
#nrsa<-nrsa %>%
#  select(-c("X.x","X.y"))
length(unique(nrsa$SITE_ID)) #n=2069 out of 2267

# AGGREGATE ECOREGIONS
nrsa$AG_ECO5<-nrsa$AG_ECO9
nrsa<-nrsa %>%
  mutate(AG_ECO5 = case_when (
    (AG_ECO9 %in% c('WMT','XER'))~'West',
    (AG_ECO9 %in% c('NPL','SPL'))~'Great Plains',
    (AG_ECO9 %in% c('UMW','TPL'))~'Midwest',
    (AG_ECO9 %in% c('NAP','SAP'))~'Appalachians',
    (AG_ECO9 %in% c('CPL'))~'Coastal Plains'))
table(nrsa$AG_ECO5)

nrsa$AG_ECO5<-ordered(nrsa$AG_ECO5, levels=c("West", "Great Plains","Midwest",
                                             "Appalachians","Coastal Plains"))

####################
# Drop duplicate observations and retain first observation (even though isotope values are different)
# https://www.codecademy.com/courses/learn-dplyr/lessons/r-data-cleaning/exercises/duplicates#:~:text=To%20check%20for%20duplicates%2C%20we,which%20rows%20are%20duplicate%20rows.&text=We%20can%20see%20that%20the,calories%2C%20is%20a%20duplicate%20row.
# Retains first observation n = 2261 compared to n=2267
nrsa_proc= nrsa%>%
  distinct(SITE_ID,VISIT_NO, .keep_all=TRUE)

##################
# Merge processed NRSA and StreamCat datasets together using left_join
nrsa_strmcatv1<-left_join(nrsa_proc,strcat,
                        by="SITE_ID")

#############
## DATA PROCESSING
# Need to match survey climate vars to year of survey (originally in land_org dataset)
# Create a YEAR column
# Convert DATE_COL from a character to a date
#nrsa_strmcatv1$DATE_COL<-as.Date(nrsa_strmcatv1$DATE_COL, format="%m/%d/%Y")
#nrsa_strmcatv1$YEAR <-format(nrsa_strmcatv1$DATE_COL,"%Y")

# SUBSET DATA BY YEAR
first<-nrsa_strmcatv1%>%
  filter(YEAR=="2013")

second<-nrsa_strmcatv1%>%
  filter(YEAR=="2014")

# Create column of survey specific climate and water temp vars that can be joined together
# 2013 n = 1018
first<-first%>%
  mutate(
    MAST_SY=MAST_2013,
    MSST_SY=MSST_2013,
    MWST_SY=MWST_2013,
    PSUMPY_SY_PT=PSUMPY_2013_PT,
    PSUMPY_SY_WS=PSUMPY_2013Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2013_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2013Ws,
    TMEAN_WSY_PT=TMEAN_W_2013_PT,
    TMEAN_WSY_WS=TMEAN_W_2013Ws,
    TMEAN_SSY_PT=TMEAN_S_2013_PT,
    TMEAN_SSY_WS=TMEAN_S_2013Ws,
    TMEAN_SY_PT=TMEAN_SY_2013_PT,
    TMEAN_SY_WS=TMEAN_SY_2013Ws)%>%
  select(-c(MAST_2013,MSST_2013,MWST_2013,PSUMPY_2013_PT,PSUMPY_2013Ws,
            TMEAN_PW_2013_PT,TMEAN_PW_2013Ws,TMEAN_W_2013_PT,TMEAN_W_2013Ws,
            TMEAN_S_2013_PT,TMEAN_S_2013Ws,TMEAN_SY_2013_PT,TMEAN_SY_2013Ws,
            MAST_2014,MSST_2014,MWST_2014,PSUMPY_2014_PT,PSUMPY_2014Ws,
            TMEAN_PW_2014_PT,TMEAN_PW_2014Ws,TMEAN_W_2014_PT,TMEAN_W_2014Ws,
            TMEAN_S_2014_PT,TMEAN_S_2014Ws,TMEAN_SY_2014_PT,TMEAN_SY_2014Ws))
names(first)


# 2014 n = 1243
second<-second%>%
  mutate(
    MAST_SY=MAST_2014,
    MSST_SY=MSST_2014,
    MWST_SY=MWST_2014,
    PSUMPY_SY_PT=PSUMPY_2014_PT,
    PSUMPY_SY_WS=PSUMPY_2014Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2014_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2014Ws,
    TMEAN_WSY_PT=TMEAN_W_2014_PT,
    TMEAN_WSY_WS=TMEAN_W_2014Ws,
    TMEAN_SSY_PT=TMEAN_S_2014_PT,
    TMEAN_SSY_WS=TMEAN_S_2014Ws,
    TMEAN_SY_PT=TMEAN_SY_2014_PT,
    TMEAN_SY_WS=TMEAN_SY_2014Ws)%>%
  select(-c(MAST_2013,MSST_2013,MWST_2013,PSUMPY_2013_PT,PSUMPY_2013Ws,
            TMEAN_PW_2013_PT,TMEAN_PW_2013Ws,TMEAN_W_2013_PT,TMEAN_W_2013Ws,
            TMEAN_S_2013_PT,TMEAN_S_2013Ws,TMEAN_SY_2013_PT,TMEAN_SY_2013Ws,
            MAST_2014,MSST_2014,MWST_2014,PSUMPY_2014_PT,PSUMPY_2014Ws,
            TMEAN_PW_2014_PT,TMEAN_PW_2014Ws,TMEAN_W_2014_PT,TMEAN_W_2014Ws,
            TMEAN_S_2014_PT,TMEAN_S_2014Ws,TMEAN_SY_2014_PT,TMEAN_SY_2014Ws))
names(second)

## COMBINE SUBSETS TOGETHER USING ROWBIND
nrsa_strmcat_proc<-bind_rows(first,second)
#n = 2261 w/345 variables
table(nrsa_strmcat_proc$VISIT_NO)
#   1    2
# 2069  192

# SUBSET ONLY VISIT_NO = 1 n = 2069 sites
nrsa_strmcat1<-nrsa_strmcat_proc %>%
  filter(VISIT_NO==1)
length(unique(nrsa_strmcat1$SITE_ID)) # n = 2069

# EXPORT DATA AS .csv files
write.csv(nrsa_strmcat_proc,"data_processed/nrsa1314/nrsa1314_strmcat_all.csv", row.names=FALSE)
write.csv(nrsa_strmcat1,"data_processed/nrsa1314/nrsa1314_strmcat_visit1.csv", row.names=FALSE)
#colnames(nrsa_strmcat1)
dat_names<-data.frame(colnames(nrsa_strmcat1))
write.csv(dat_names,"data_processed/nrsa1314/column_vars1314.csv", row.names=FALSE)

# Store processed data within the package
usethis::use_data(nrsa_strmcat1,overwrite=TRUE)

usethis::use_data(nrsa_strmcat_proc,overwrite=TRUE)


################################
## PROCESS DATA TO BE ABLE TO MERGE WITH OTHER SURVEYS
## READ PROCESSED DATA n = 2261
nrsa_strmcat_proc<-read.csv("data_processed/nrsa1314/nrsa1314_strmcat_all.csv")

## # LANDCOVER/USE CLASSES
nrsa_strmcat_proc <-nrsa_strmcat_proc %>%
  mutate(PCTOW_WS=PctOw2013Ws,
         PCTICE_WS=PctIce2013Ws,
         PCTURBOP_WS=PctUrbOp2013Ws,
         PCTURBLO_WS=PctUrbLo2013Ws,
         PCTURBMD_WS=PctUrbMd2013Ws,
         PCTURBHI_WS=PctUrbHi2013Ws,
         PCTDECID_WS=PctDecid2013Ws,
         PCTCONIF_WS=PctConif2013Ws,
         PCTMXFST_WS=PctMxFst2013Ws,
         PCTSHRB_WS=PctShrb2013Ws,
         PCTGRS_WS=PctGrs2013Ws,
         PCTHAY_WS=PctHay2013Ws,
         PCTCROP_WS=PctCrop2013Ws,
         PCTWDWET_WS=PctWdWet2013Ws,
         PCTHBWET_WS=PctHbWet2013Ws,
         PCTOW_WsRp100=PctOw2013_WsRp100,
         PCTICE_WsRp100=PctIce2013_WsRp100,
         PCTURBOP_WsRp100=PctUrbOp2013_WsRp100,
         PCTURBLO_WsRp100=PctUrbLo2013_WsRp100,
         PCTURBMD_WsRp100=PctUrbMd2013_WsRp100,
         PCTURBHI_WsRp100=PctUrbHi2013_WsRp100,
         PCTDECID_WsRp100=PctDecid2013_WsRp100,
         PCTCONIF_WsRp100=PctConif2013_WsRp100,
         PCTMXFST_WsRp100=PctMxFst2013_WsRp100,
         PCTSHRB_WsRp100=PctShrb2013_WsRp100,
         PCTGRS_WsRp100=PctGrs2013_WsRp100,
         PCTHAY_WsRp100=PctHay2013_WsRp100,
         PCTCROP_WsRp100=PctCrop2013_WsRp100,
         PCTWDWET_WsRp100=PctWdWet2013_WsRp100,
         PCTHBWET_WsRp100=PctHbWet2013_WsRp100,
         PCTIMP_WS=PctImp2013Ws,
         PCTIMP_WsRp100=PctImp2013_RipBuf100WsRp100)%>%
  select(-c(PctOw2013Ws, PctIce2013Ws, PctUrbOp2013Ws, PctUrbLo2013Ws, PctUrbMd2013Ws, PctUrbHi2013Ws,
            PctDecid2013Ws, PctConif2013Ws, PctMxFst2013Ws, PctShrb2013Ws, PctGrs2013Ws,
            PctHay2013Ws, PctCrop2013Ws, PctWdWet2013Ws, PctHbWet2013Ws,
            PctOw2013_WsRp100, PctIce2013_WsRp100, PctUrbOp2013_WsRp100,
            PctUrbLo2013_WsRp100, PctUrbMd2013_WsRp100, PctUrbHi2013_WsRp100,
            PctDecid2013_WsRp100, PctConif2013_WsRp100, PctMxFst2013_WsRp100,
            PctShrb2013_WsRp100, PctGrs2013_WsRp100, PctHay2013_WsRp100, PctCrop2013_WsRp100,
            PctWdWet2013_WsRp100, PctHbWet2013_WsRp100, PctImp2013Ws,
            PctImp2013_RipBuf100WsRp100))


nrsa1314<-nrsa_strmcat_proc%>%
  select(c("SITE_ID","VISIT_NO","DATE_COL","YEAR", "SITETYPE","STATE","AG_ECO3","AG_ECO9","AG_ECO5",
           "US_L3CODE","US_L4CODE",
           "LAT_DD83","LON_DD83","PROTOCOL","REALM","STRAH_ORD",
           "MMI_BENT","OE_SCORE_OLD","OE_SCORE",
           "AMMONIA_N_RESULT","ANC_RESULT","CHLORIDE_RESULT","COLOR_RESULT","COND_RESULT","DOC_RESULT","MAGNESIUM_RESULT","SODIUM_RESULT",
           "POTASSIUM_RESULT","NTL_RESULT","PTL_RESULT","SULFATE_RESULT","TSS_RESULT","TURB_RESULT",
           "RT_WQI","CL_pt","SO4_pt","PTL_pt","NTL_pt","TURB_pt","ENTERO_PT", "DO_PT","PH_PT","WQII",
           "H2O_dD","H2O_d18O","d.excess","MAST_SY","MSST_SY","MWST_SY",
           "LDCBF_G08",
           "XDEPTH_CM","SDDEPTH_CM","XWXD","RP100","XBKF_W","XBKF_H","XINC_H","SINU","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SAFN","PCT_SFGF","LDMB_BW5","LRBS_BW5","LRBS_G08","PCT_FAST","PCT_SLOW",
           "RPRAT","PCT_BDRK","XFC_ALG","XFC_AQM","XFC_LWD","XFC_NAT","V1W_MSQ","XCDENBK","XCDENMID",
           "XCL","XGB","XC","XCMGW","QR1","QRVEG1","RDIST1","W1_HALL","W1H_WALL","W1_HNOAG","W1_HAG",
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
           "AgKffactWs","FertWs","ManureWs","NPDESDensWs","NPDESDensWsRp100"))
#n = 2261 (visits 1 & 2) with 180 variables

#######################
#GET COLUMN NAMES TO CHECK
dat_names_share<-data.frame(colnames(nrsa1314))
write.csv(dat_names_share,"data_processed/nrsa1314/column_varsSHARE_1314.csv", row.names=FALSE)

## WRITE TO CSV
write.csv(nrsa1314,"data_processed/nrsa1314/nrsa1314_to_merge.csv", row.names=FALSE)






#######################
## OLD - CHECK LISTS
# https://stackoverflow.com/questions/23353067/comparing-two-lists-r
length(unique(nrsa_proc$SITE_ID))
#create vectors combining siteID and visitno
org1<-site_org%>%
  unite('ID',SITE_ID,VISIT_NO)
org<-unlist(org1$ID) #n=2261

proc1<-nrsa_proc%>%
  unite('ID',SITE_ID,VISIT_NO)
proc<-unlist(proc1$ID)#n=2267

#See if in both
both<-org %in% proc
table(both)

#What is in both
int <-intersect(org,proc)
int #2069

# What is different in original - nothing
difs <-setdiff(org, proc)
difs
# And what is different in processed - nothing
difs2 <-setdiff(proc,org)
difs2

# Check for duplicate site-id and visit no
duplic<-nrsa_proc%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)
# There are 11 observations that are duplicates for site and visit
# in isotope dataset - 2 each:  OKLS-1176,WILS-1089,WIS9-0935,FLS9-0921
# in phab_oe dataset - 3 times: MORM-1002

####################
# Drop duplicate observations and retain first observation (even though isotope values are different)
# https://www.codecademy.com/courses/learn-dplyr/lessons/r-data-cleaning/exercises/duplicates#:~:text=To%20check%20for%20duplicates%2C%20we,which%20rows%20are%20duplicate%20rows.&text=We%20can%20see%20that%20the,calories%2C%20is%20a%20duplicate%20row.
# Retains first observation n = 2261 compared to n=2267
nrsa_proc2= nrsa_proc%>%
  distinct(SITE_ID,VISIT_NO, .keep_all=TRUE)

#####################
# SUBSET ONLY VISIT_NO = 1 n = 2069 lakes
nrsa1<-nrsa_proc2 %>%
  filter(VISIT_NO==1)
length(unique(nrsa1$SITE_ID)) # n = 2069

test=nrsa1%>%
  group_by(SITE_ID)%>%
  filter(n()>1)

# Store processed data within the package
usethis::use_data(nrsa1,overwrite=TRUE)

usethis::use_data(nrsa_proc,overwrite=TRUE)

# EXPORE DATA AS .csv files
write.csv(nrsa_proc2,"data_processed/nrsa_all.csv")
write.csv(nrsa1,"data_processed/nrsa_visit1.csv")


