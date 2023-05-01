################
## LOAD NRSA DATA
###########
# LOAD DATA - NRSA 2018-19 from website

rm(list=ls())

# Libraries
library(dplyr)
library(tidyr)

library(devtools)
devtools::load_all("./") # Loads current directory
library(SEMNRSA)

# LOAD data stored in Rpackage - a .rda file
#load(file="data/nrsa.rda")

# ORIGINAL NRSA 2018-19 DATA downloaded from website 1/21/2022 and3/
#siteinformation_wide,nrsa1314_widechem,bentmmi, landmet, phabmed

# On OneDrive
site_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1819_website/nrsa-1819-site-information-data-updated.csv")
chem_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1819_website/nrsa_1819_water_chemistry_chla_-_data.csv")
#bent_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/bentcond.csv")
#land_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/land.csv")
phab_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1819_website/nrsa_1819_physical_habitat_larger_set_of_metrics_-_data.csv")
strcat_org <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/StreamCat/NRSA_2018_19/FINAL_TABLE_NRSA_1819.csv")

#chem_org2 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/fieldchemmeasure.csv")
#site_org2 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/ext_siteinfo.csv")

# Load other NRSA data compiled across surveys
benthic_oe_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_benthic_indices/NRSA2008-2019_OE_Scores.csv")
isotope_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/Water_isotope/NRSA 2018 2019 Water Isotope.csv")
phab_oe_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_PHab_Discharge/NRSA_PHab_Discharge_OE1819.csv")
wqii_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/WQII/wqii_nrsa.csv")

# FORMAT DATE IN SITE
site_org$DATE_COL<-as.Date(site_org$DATE_COL, format="%m/%d/%Y")
str(site_org$DATE_COL)
site_org$YEAR <-format(site_org$DATE_COL,"%Y")
table(site_org$YEAR)
# 2018 2019
# 1044 1068
# Remove UID - this may be old? - will replace with UID from benthic O/E dataset Karen B shared
site_org<-site_org%>%
  select(!UID)

# Reduce datasets to merge together
chem <- chem_org%>%
  select(c("SITE_ID","VISIT_NO",
               "AMMONIA_N_RESULT",'AMMONIA_N_UNITS',
               "ANC_RESULT","ANC_UNITS",
               "CALCIUM_RESULT","CALCIUM_UNITS",
               "CHLORIDE_RESULT","CHLORIDE_UNITS",
               "COLOR_RESULT", "COLOR_UNITS",
               "COND_RESULT","COND_UNITS",
               "DOC_RESULT","DOC_UNITS",
               "MAGNESIUM_RESULT","MAGNESIUM_UNITS",
               "NITRATE_N_RESULT","NITRATE_N_UNITS",
               "NITRATE_NITRITE_N_RESULT","NITRATE_NITRITE_N_UNITS",
               "NITRITE_N_RESULT","NITRITE_N_UNITS",
               "NTL_RESULT", "NTL_UNITS",
               "PH_RESULT","PH_UNITS",
               "POTASSIUM_RESULT","POTASSIUM_UNITS",
               "PTL_RESULT","PTL_UNITS",
               "SILICA_RESULT","SILICA_UNITS",
               "SODIUM_RESULT", "SODIUM_UNITS",
               "SULFATE_RESULT","SULFATE_UNITS",
               "TSS_RESULT","TSS_UNITS",
               "TURB_RESULT", "TURB_UNITS"))

phab <-phab_org%>%
  select(c("SITE_ID","VISIT_NO","PROTOCOL","XDEPTH_CM","SDDEPTH_CM",'XWIDTH',"XWXD","RP100","RPGT50","XBKF_W","XBKF_H","XINC_H","SINU","XSLOPE","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SA","PCT_SAFN","PCT_SFGF",
           "LDCBF_G08","LDMB_BW5","LRBS_BW5","LRBS_G08",
           "PCT_FAST","PCT_POOL","PCT_SLOW",
           "RpRat","RPMXDEP_CM","RPXDEP_CM",
           "PCT_BDRK",
           "XFC_ALG","XFC_AQM","XFC_LWD","XFC_BRS", "XFC_OHV","XFC_BIG","XFC_NAT","XFC_UCB","V1W_MSQ",
           "XCDENBK","XCDENMID","XCL","XGB","XGW","XMW","XC","XCS","XCL","XCM","XCMGW","XPCAN","XPCM","XPCMG",
           "QR1","QRVeg1","RDIST1",
           "W1_HALL","W1H_WALL","W1H_LOG","W1_HNOAG","W1_HAG"))

# NOT IN 2018-19 "RB3","REYP3","RRPW3","RPRAT","QRVEG1",


strcat <-strcat_org%>%
  select("SITE_ID",
         "CatAreaSqKm","WsAreaSqKm","CatAreaSqKmRp100","WsAreaSqKmRp100",
         "HydrlCondWs","OMHWs",
         "RdDensWs","RdDensWsRp100","RdDens_WS_PctFull","RdDens_RipBuf100_WS_PctFullRp100",
         "DamDensWs","DamNIDStorWs","DamNrmStorWs",
         "NABD_DensWs","NABD_NIDStorWs","NABD_NrmStorWs",
         "MineDensWs","MineDensWsRp100",
         "AgKffactWs","FertWs","ManureWs","NPDESDensWs","NPDESDensWsRp100",
         "SuperfundDensWs",
         "HUDen2010Ws","HUDen2010WsRp100","PopDen2010Ws","PopDen2010WsRp100",
         "PctOw2019Ws","PctIce2019Ws","PctUrbOp2019Ws","PctUrbLo2019Ws","PctUrbMd2019Ws","PctUrbHi2019Ws",
         "PctDecid2019Ws","PctConif2019Ws","PctMxFst2019Ws",
         "PctShrb2019Ws","PctGrs2019Ws",
         "PctHay2019Ws","PctCrop2019Ws",
         "PctWdWet2019Ws","PctHbWet2019Ws",
         "PctOw2019_WsRp100","PctIce2019_WsRp100","PctUrbOp2019_WsRp100","PctUrbLo2019_WsRp100","PctUrbMd2019_WsRp100","PctUrbHi2019_WsRp100",
         "PctDecid2019_WsRp100","PctConif2019_WsRp100","PctMxFst2019_WsRp100",
         "PctShrb2019_WsRp100","PctGrs2019_WsRp100",
         "PctHay2019_WsRp100","PctCrop2019_WsRp100",
         "PctWdWet2019_WsRp100","PctHbWet2019_WsRp100",
         "PctFrstLoss2013Ws","PctFrstLoss2013WsRp100",
         "PctImp2019Ws","PctImp2019_RipBuf100WsRp100",
         "BFIWs","BFI_WS_PctFull",
         "PctNOT_PRINCIPLEWs","PctSANDSTONEWs","PctSEMICONSOL_SANDWs","PctVOLCANICWs",
         "PctSANDSTONE_CARBONATEWs","PctUNCONSOL_SAND_GRAVELWs","PctCARBONATEWs",
         "ClayWs",'PctCarbResidWs',"PctNonCarbResidWs","PctAlkIntruVolWs","PctSilicicWs",
         "PctExtruVolWs","PctColluvSedWs",
         "PctGlacTilClayWs","PctGlacTilLoamWs","PctGlacTilCrsWs","PctGlacLakeCrsWs","PctGlacLakeFineWs",
         "PctHydricWs","PctEolCrsWs","PctEolFineWs","PctSalLakeWs","PctAlluvCoastWs","PctCoastCrsWs","PctWaterWs",
         "ElevWs","PermWs","KffactWs","Kffact_PT","LSTFRZWs","LSTFRZ_PT","RDHWs","RDH_PT","RHMEANWs","RHMEAN_PT",
         "RunoffWs","WDMAX_PT","WDMIN_PT","WDSUM_PT",
         "PMAXWs","PMINWs","Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws",
         "TMEAN_S_2018Ws","TMEAN_S_2018_PT","TMEAN_S_2019Ws","TMEAN_S_2019_PT","TMEAN_W_2018Ws","TMEAN_W_2018_PT",
         "TMEAN_W_2019Ws","TMEAN_W_2019_PT","TMEAN_PW_2018Ws","TMEAN_PW_2018_PT","TMEAN_PW_2019Ws","TMEAN_PW_2019_PT",
         "TMEAN_SY_2018Ws","TMEAN_SY_2018_PT","TMEAN_SY_2019Ws","TMEAN_SY_2019_PT",
         "PSUMPY_2018Ws","PSUMPY_2018_PT","PSUMPY_2019Ws","PSUMPY_2019_PT",
         "MAST_2018","MAST_2019","MSST_2018","MSST_2019","MWST_2018","MWST_2019")

# Updated benthic O/E
# PROCESS DATA - SELECT YEAR 2018-19
# DATE_COL from a character to a date
benthic_oe_org$DATE_COL<-as.Date(benthic_oe_org$DATE_COL, format="%m/%d/%Y")
benthic_oe_org$YEAR <-format(benthic_oe_org$DATE_COL,"%Y")
benthic_oe <-benthic_oe_org%>%
  filter(YEAR==2018|YEAR==2019)%>%
  select(c("UID","SITE_ID","VISIT_NO","OE_SCORE")) #n=2105

#PHab O/E
phab_oe <- phab_oe_org%>%
  select(c("SITE_ID","VISIT_NO","REALM","W1H_CROP",
           "XSLOPE_use","XWIDTH_use","Lpt01_XCMGW","Lpt01_XFC_NAT",
           "LRBS_use","RDIST_COND","LRBS_Cond_use","LOE_RBS_use",
           "LXCMGW_Cond_use","LOE_XCMGW_use","LXFC_NAT_Cond_use","LOE_XFC_NAT_use",
           "Qbkf_cl","LQbkf_cl","Qbkf_kmcl","LQbkf_kmcl","QLow_cl","LQLow_cl",
           "QLow_cl","LQLow_cl","QLow_kmcl","LQLow_kmcl","LOE_QLow_cl","LOE_Qbkf_cl"
  ))

length(unique(phab_oe$SITE_ID))#n=1919

# Check for duplicate obs n = 4, 2each: SITE_ID=NRS18_MN_RF002 and NRS18_MN_RF001
test<-phab_oe%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)

# WQII
names(wqii_org)
wqii<- wqii_org%>%
  filter(YEAR==2018|YEAR==2019)%>%
  select(c("SITE_ID","VISIT_NO","RT_WQI",
           "CL_pt","SO4_pt","PTL_pt","NTL_pt",
           "TURB_pt","ENTERO_PT",
           "DO_PT","PH_PT","NUMVARN","WQII"))

length(unique(wqii$SITE_ID)) #n=1919 out of 2112 observations


# Water isotopes
isotope_org$DATE_COL<-as.Date(isotope_org$DATE_COL, format="%m/%d/%Y")
isotope_org$YEAR <-format(isotope_org$DATE_COL,"%Y")
table(isotope_org$YEAR)
#2018 2019
# 960  993
isotope <- isotope_org%>%
  select(c("SITE_ID","VISIT_NO","SAMPLE_ID","H2O_dD","H2O_d18O","d.excess"))
length(unique(isotope$SITE_ID))#1768

# Check for duplicates n=4 (2 each but have different isotope values)
# NRS18_MN_RF001,NRS18_MN_RF002
test<-isotope%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)

# WRITE REDUCED DATASETS TO NEW FOLDER TO MERGE
write.csv(site_org,"data_to_merge_1819/a_site.csv",row.names=FALSE)
#write.csv(bent,"data_to_merge_1819/b_benth.csv")
write.csv(benthic_oe,"data_to_merge_1819/b_benth_oe.csv",row.names=FALSE)
write.csv(chem,"data_to_merge_1819/c_chem_.csv",row.names=FALSE)
#write.csv(land,"data_to_merge_1819/d_land.csv")
write.csv(phab,"data_to_merge_1819/e_phab.csv",row.names=FALSE)

write.csv(phab_oe,"data_to_merge_1819/f_phab_oe.csv",row.names=FALSE)
write.csv(wqii,"data_to_merge_1819/g_wqii.csv",row.names=FALSE)
write.csv(isotope,"data_to_merge_1819/h_isotope.csv", row.names=FALSE)

# Save in processed data folder
write.csv(strcat,"data_processed/subset_streamcat1819.csv", row.names=FALSE)


#########
## Use multi-merge function to bring data together
# Merging based on Site ID

# Need to treat SEMNRSA like a package to be able to run multimerge function
devtools::load_all()

nrsa<- multimerge("data_to_merge_1819")
names(nrsa)
#nrsa<-nrsa %>%
#  select(-c("X.x","X.y"))
length(unique(nrsa$SITE_ID)) #n=1919 out of 2364

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
# Retains first observation n = 2110 compared to n=2364
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
  filter(YEAR=="2018")

second<-nrsa_strmcatv1%>%
  filter(YEAR=="2019")

# Create column of survey specific climate and water temp vars that can be joined together
# 2018 n = 1043
first<-first%>%
  mutate(
    MAST_SY=MAST_2018,
    MSST_SY=MSST_2018,
    MWST_SY=MWST_2018,
    PSUMPY_SY_PT=PSUMPY_2018_PT,
    PSUMPY_SY_WS=PSUMPY_2018Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2018_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2018Ws,
    TMEAN_WSY_PT=TMEAN_W_2018_PT,
    TMEAN_WSY_WS=TMEAN_W_2018Ws,
    TMEAN_SSY_PT=TMEAN_S_2018_PT,
    TMEAN_SSY_WS=TMEAN_S_2018Ws,
    TMEAN_SY_PT=TMEAN_SY_2018_PT,
    TMEAN_SY_WS=TMEAN_SY_2018Ws)%>%
  select(-c(MAST_2018,MSST_2018,MWST_2018,PSUMPY_2018_PT,PSUMPY_2018Ws,
            TMEAN_PW_2018_PT,TMEAN_PW_2018Ws,TMEAN_W_2018_PT,TMEAN_W_2018Ws,
            TMEAN_S_2018_PT,TMEAN_S_2018Ws,TMEAN_SY_2018_PT,TMEAN_SY_2018Ws,
            MAST_2019,MSST_2019,MWST_2019,PSUMPY_2019_PT,PSUMPY_2019Ws,
            TMEAN_PW_2019_PT,TMEAN_PW_2019Ws,TMEAN_W_2019_PT,TMEAN_W_2019Ws,
            TMEAN_S_2019_PT,TMEAN_S_2019Ws,TMEAN_SY_2019_PT,TMEAN_SY_2019Ws))
names(first)


# 2019 n = 1067
second<-second%>%
  mutate(
    MAST_SY=MAST_2019,
    MSST_SY=MSST_2019,
    MWST_SY=MWST_2019,
    PSUMPY_SY_PT=PSUMPY_2019_PT,
    PSUMPY_SY_WS=PSUMPY_2019Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2019_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2019Ws,
    TMEAN_WSY_PT=TMEAN_W_2019_PT,
    TMEAN_WSY_WS=TMEAN_W_2019Ws,
    TMEAN_SSY_PT=TMEAN_S_2019_PT,
    TMEAN_SSY_WS=TMEAN_S_2019Ws,
    TMEAN_SY_PT=TMEAN_SY_2019_PT,
    TMEAN_SY_WS=TMEAN_SY_2019Ws)%>%
  select(-c(MAST_2018,MSST_2018,MWST_2018,PSUMPY_2018_PT,PSUMPY_2018Ws,
            TMEAN_PW_2018_PT,TMEAN_PW_2018Ws,TMEAN_W_2018_PT,TMEAN_W_2018Ws,
            TMEAN_S_2018_PT,TMEAN_S_2018Ws,TMEAN_SY_2018_PT,TMEAN_SY_2018Ws,
            MAST_2019,MSST_2019,MWST_2019,PSUMPY_2019_PT,PSUMPY_2019Ws,
            TMEAN_PW_2019_PT,TMEAN_PW_2019Ws,TMEAN_W_2019_PT,TMEAN_W_2019Ws,
            TMEAN_S_2019_PT,TMEAN_S_2019Ws,TMEAN_SY_2019_PT,TMEAN_SY_2019Ws))
names(second)

## COMBINE SUBSETS TOGETHER USING ROWBIND
nrsa_strmcat_proc<-bind_rows(first,second)
#n = 2110 w/335 variables
table(nrsa_strmcat_proc$VISIT_NO)
# 1    2    R
#1919  189    2

# SUBSET ONLY VISIT_NO = 1 n = 1919 sites
nrsa_strmcat1<-nrsa_strmcat_proc %>%
  filter(VISIT_NO==1)
length(unique(nrsa_strmcat1$SITE_ID)) # n = 1919

# EXPORE DATA AS .csv files
write.csv(nrsa_strmcat_proc,"data_processed/nrsa1819/nrsa1819_strmcat_all.csv", row.names=FALSE)
write.csv(nrsa_strmcat1,"data_processed/nrsa1819/nrsa1819_strmcat_visit1.csv", row.names=FALSE)
#colnames(nrsa_strmcat1)
dat_names<-data.frame(colnames(nrsa_strmcat1))
write.csv(dat_names,"data_processed/nrsa1819/column_vars1819.csv", row.names=FALSE)


################################
## PROCESS DATA TO BE ABLE TO MERGE WITH OTHER SURVEYS
## READ PROCESSED DATA n = 2110
nrsa_strmcat_proc<-read.csv("data_processed/nrsa1819/nrsa1819_strmcat_all.csv")

## # LANDCOVER/USE CLASSES
nrsa_strmcat_proc <-nrsa_strmcat_proc %>%
  mutate(PCTOW_WS=PctOw2019Ws,
         PCTICE_WS=PctIce2019Ws,
         PCTURBOP_WS=PctUrbOp2019Ws,
         PCTURBLO_WS=PctUrbLo2019Ws,
         PCTURBMD_WS=PctUrbMd2019Ws,
         PCTURBHI_WS=PctUrbHi2019Ws,
         PCTDECID_WS=PctDecid2019Ws,
         PCTCONIF_WS=PctConif2019Ws,
         PCTMXFST_WS=PctMxFst2019Ws,
         PCTSHRB_WS=PctShrb2019Ws,
         PCTGRS_WS=PctGrs2019Ws,
         PCTHAY_WS=PctHay2019Ws,
         PCTCROP_WS=PctCrop2019Ws,
         PCTWDWET_WS=PctWdWet2019Ws,
         PCTHBWET_WS=PctHbWet2019Ws,
         PCTOW_WsRp100=PctOw2019_WsRp100,
         PCTICE_WsRp100=PctIce2019_WsRp100,
         PCTURBOP_WsRp100=PctUrbOp2019_WsRp100,
         PCTURBLO_WsRp100=PctUrbLo2019_WsRp100,
         PCTURBMD_WsRp100=PctUrbMd2019_WsRp100,
         PCTURBHI_WsRp100=PctUrbHi2019_WsRp100,
         PCTDECID_WsRp100=PctDecid2019_WsRp100,
         PCTCONIF_WsRp100=PctConif2019_WsRp100,
         PCTMXFST_WsRp100=PctMxFst2019_WsRp100,
         PCTSHRB_WsRp100=PctShrb2019_WsRp100,
         PCTGRS_WsRp100=PctGrs2019_WsRp100,
         PCTHAY_WsRp100=PctHay2019_WsRp100,
         PCTCROP_WsRp100=PctCrop2019_WsRp100,
         PCTWDWET_WsRp100=PctWdWet2019_WsRp100,
         PCTHBWET_WsRp100=PctHbWet2019_WsRp100,
         PCTIMP_WS=PctImp2019Ws,
         PCTIMP_WsRp100=PctImp2019_RipBuf100WsRp100)%>%
  select(-c(PctOw2019Ws, PctIce2019Ws, PctUrbOp2019Ws, PctUrbLo2019Ws, PctUrbMd2019Ws, PctUrbHi2019Ws,
            PctDecid2019Ws, PctConif2019Ws, PctMxFst2019Ws, PctShrb2019Ws, PctGrs2019Ws,
            PctHay2019Ws, PctCrop2019Ws, PctWdWet2019Ws, PctHbWet2019Ws,
            PctOw2019_WsRp100, PctIce2019_WsRp100, PctUrbOp2019_WsRp100,
            PctUrbLo2019_WsRp100, PctUrbMd2019_WsRp100, PctUrbHi2019_WsRp100,
            PctDecid2019_WsRp100, PctConif2019_WsRp100, PctMxFst2019_WsRp100,
            PctShrb2019_WsRp100, PctGrs2019_WsRp100, PctHay2019_WsRp100, PctCrop2019_WsRp100,
            PctWdWet2019_WsRp100, PctHbWet2019_WsRp100, PctImp2019Ws,
            PctImp2019_RipBuf100WsRp100))


nrsa1819<-nrsa_strmcat_proc%>%
  select(c("UID","SITE_ID","VISIT_NO","DATE_COL","YEAR","SITETYPE","STATE_NM","AG_ECO3","AG_ECO9","AG_ECO5",
           "US_L3CODE","US_L4CODE","HUC8",
           "LAT_DD83","LON_DD83","PROTOCOL","REALM","STRAH_ORD",
           "OE_SCORE",
           "AMMONIA_N_RESULT","ANC_RESULT","CHLORIDE_RESULT","COLOR_RESULT","COND_RESULT","DOC_RESULT",
           "MAGNESIUM_RESULT","SODIUM_RESULT","POTASSIUM_RESULT","NITRATE_N_RESULT","NITRITE_N_RESULT",
           "NTL_RESULT","PTL_RESULT","SULFATE_RESULT","TSS_RESULT","TURB_RESULT",
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
#n = 2110 (visits 1 & 2) with 187 variables

#"RPRAT","QRVEG1","MMI_BENT","OE_SCORE_OLD"

#######################
#GET COLUMN NAMES TO CHECK
dat_names_share<-data.frame(colnames(nrsa1819))
write.csv(dat_names_share,"data_processed/nrsa1819/column_varsSHARE_1819.csv", row.names=FALSE)

## WRITE TO CSV
write.csv(nrsa1819,"data_processed/nrsa1819/nrsa1819_to_merge.csv", row.names=FALSE)

