################
## LOAD NRSA DATA
###########
# LOAD DATA - NRSA 2008-09 from website

rm(list=ls())

# Libraries
library(dplyr)
library(tidyr)
library(naniar)

library(devtools)
devtools::load_all("./") # Loads current directory
library(SEMNRSA)

# LOAD data stored in Rpackage - a .rda file
#load(file="data/nrsa.rda")

# ORIGINAL NRSA 2013-14 DATA downloaded from website 8/16/21
#siteinformation_wide,nrsa1314_widechem,bentmmi, landmet, phabmed

# On OneDrive
site_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/siteinfo_0.csv")
chem_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/chem.csv")
bent_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/bentcond.csv")
land_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/land.csv")
phab_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/phablow.csv")
strcat_org <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/StreamCat/NRSA_2008_09/FINAL_TABLE_NRSA_0809.csv")

# Transect conductivity, DO, pH, temperature...
#chem_org2 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/fieldchemmeasure.csv")
# NOT SURE WHAT THIS IS? - spans from 1978-2011 and has for 2008 only n=35 and for 2009 n=51
#site_org2 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/ext_siteinfo.csv")

# Load other NRSA data
benthic_oe_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_benthic_indices/NRSA2008-2019_OE_Scores.csv")
isotope_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/Water_isotope/NRSA 0809 Water Isotope.csv")
phab_oe_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_PHab_Discharge/NRSA_PHab_Discharge_OE891314.csv")
wqii_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/WQII/wqii_nrsa.csv")

# FORMAT DATE IN SITE
site_org$DATE_COL<-as.Date(site_org$DATE_COL, format="%d-%b-%y")
summary(site_org$DATE_COL)
str(site_org$DATE_COL)
site_org$YEAR <-format(site_org$DATE_COL,"%Y")
table(site_org$YEAR)
#2008 2009
#868 1452
# Remove UID - this may be old? - will replace with UID from benthic O/E dataset Karen B shared
site_org<-site_org%>%
  select(!UID)

# Reduce datasets to merge together
chem_vars <- c("SITE_ID","VISIT_NO",
               "NH4","NH4_ALERT",
               "ANC","ANC_ALERT",
               "CA","CA_ALERT",
               "CL","CL_ALERT",
               "COLOR", "COLOR_ALERT",
               "COND","COND_ALERT",
               "DOC","DOC_ALERT",
               "MG","MG_ALERT",
               "SODIUM","SODIUM_ALERT",
               "K","K_ALERT",
               "NO3","NO3_ALERT",
               "NO3NO2","NO3NO2_ALERT",
               "NO2","NO2_ALERT",
               "NTL", "NTL_ALERT",
               "PHLAB","PHLAB_ALERT",
               "PTL","PTL_ALERT",
               "SIO2","SIO2_ALERT",
               "SO4","SO4_ALERT",
               "TSS","TSS_ALERT",
               "TURB", "TURB_ALERT")
chem <-chem_org[chem_vars]

bent<-bent_org%>%
  rename("OE_SCORE_OLD"="OE_SCORE")%>%
  select(c("SITE_ID","VISIT_NO","MMI_BENT","OE_SCORE_OLD"))

#land<-land_org%>%
#  select(-c("PUBLICATION_DATE","UID","DATE_COL",
#            "SITETYPE","INDEX_VISIT",'LAT_DD83',"LON_DD83"))

phab <-phab_org%>%
  select(c("SITE_ID","VISIT_NO","PROTOCOL","XDEPTH_CM","SDDEPTH_CM",'XWIDTH',"XWXD","RP100","XBKF_W","XBKF_H","XINC_H","SINU","XSLOPE","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SAFN","PCT_SFGF",
           "LDCBF_G08","LDMB_BW5","LRBS_BW5","LRBS_G08",
           "PCT_FAST","PCT_SLOW",
           "RPRAT",
           "PCT_BDRK",
           "XFC_ALG","XFC_AQM","XFC_LWD","XFC_NAT","V1W_MSQ",
           "XCDENBK","XCDENMID","XCDENBK","XCDENMID","XCL","XGB","XGW","XC","XCMGW",  # "XCS","XMW","XCM","XPCAN","XPCM","XPCMG",
           "QR1","QRVEG1","RDIST1",
           "W1_HALL","W1H_WALL","W1_HNOAG","W1_HAG"))
#THESE DO NOT EXIST for the 2008-09 data
#"RPGT50","PCT_SA","RB3","REYP3","RRPW3","PCT_POOL","RPXDEP_CM", "RPMXDEP_CM"
# "XFC_BRS", "XFC_OHV","XFC_BIG","XFC_UCB","XCM","XPCAN","XPCM","XPCMG","W1H_LOG",

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
         "PctOw2008Ws","PctIce2008Ws","PctUrbOp2008Ws","PctUrbLo2008Ws","PctUrbMd2008Ws","PctUrbHi2008Ws",
         "PctDecid2008Ws","PctConif2008Ws","PctMxFst2008Ws",
         "PctShrb2008Ws","PctGrs2008Ws",
         "PctHay2008Ws","PctCrop2008Ws",
         "PctWdWet2008Ws","PctHbWet2008Ws",
         "PctOw2008_WsRp100","PctIce2008_WsRp100","PctUrbOp2008_WsRp100","PctUrbLo2008_WsRp100","PctUrbMd2008_WsRp100","PctUrbHi2008_WsRp100",
         "PctDecid2008_WsRp100","PctConif2008_WsRp100","PctMxFst2008_WsRp100",
         "PctShrb2008_WsRp100","PctGrs2008_WsRp100",
         "PctHay2008_WsRp100","PctCrop2008_WsRp100",
         "PctWdWet2008_WsRp100","PctHbWet2008_WsRp100",
         "PctFrstLoss2008Ws","PctFrstLoss2008WsRp100",
         "PctFrstLoss2009Ws","PctFrstLoss2009WsRp100",
         "PctImp2008Ws","PctImp2008WsRp100",
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
         "TMEAN_S_2008Ws","TMEAN_S_2008_PT","TMEAN_S_2009Ws","TMEAN_S_2009_PT","TMEAN_W_2008Ws","TMEAN_W_2008_PT","TMEAN_W_2009Ws","TMEAN_W_2009_PT","TMEAN_PW_2008Ws","TMEAN_PW_2008_PT","TMEAN_PW_2009Ws","TMEAN_PW_2009_PT",
         "TMEAN_SY_2008Ws","TMEAN_SY_2008_PT","TMEAN_SY_2009Ws","TMEAN_SY_2009_PT",
         "PSUMPY_2008Ws","PSUMPY_2008_PT","PSUMPY_2009Ws","PSUMPY_2009_PT",
         "MAST_2008","MAST_2009","MSST_2008","MSST_2009","MWST_2008","MWST_2009")

#####################
## PROCESS DATA WITH MULTIPLE SURVEYS _ SELECT 2008-09
# Updated benthic O/E
# PROCESS DATA - SELECT YEAR 2013-14
# DATE_COL from a character to a date
benthic_oe_org$DATE_COL<-as.Date(benthic_oe_org$DATE_COL, format="%m/%d/%Y")
benthic_oe_org$YEAR <-format(benthic_oe_org$DATE_COL,"%Y")
benthic_oe <-benthic_oe_org%>%
  filter(YEAR==2008|YEAR==2009)%>%
  select(c("UID","SITE_ID","VISIT_NO","OE_SCORE"))

#PHab O/E
phab_oe <- phab_oe_org%>%
  filter(YEAR==2008|YEAR==2009)%>%
  select(c("SITE_ID","VISIT_NO","REALM","W1H_CROP",
           "XSLOPE_use","XWIDTH_use","Lpt01_XCMGW","Lpt01_XFC_NAT",
           "LRBS_use","RDIST_COND","LRBS_Cond_use","LOE_RBS_use",
           "LXCMGW_Cond_use","LOE_XCMGW_use","LXFC_NAT_Cond_use","LOE_XFC_NAT_use",
           "Qbkf_cl","LQbkf_cl","Qbkf_kmcl","LQbkf_kmcl","QLow_cl","LQLow_cl",
           "QLow_cl","LQLow_cl","QLow_kmcl","LQLow_kmcl","LOE_QLow_cl","LOE_Qbkf_cl"
  ))

length(unique(phab_oe$SITE_ID))#n=2123 out of 2320 obs

# Check for duplicate obs n = 34 SITE_ID= FW08CA020, FW08CA031, FW08IN008(v1&v2), FW08IN011, FW08IN030, FW08KS001,
# FW08KS003, FW08MO001, FW08MO004, FW08NE001, FW08NE060,FW08NY003, FW08SD063, FW08TX001,FW08TX002,FW08WA007
test<-phab_oe%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)

# WQII
names(wqii_org)
wqii<- wqii_org%>%
  filter(YEAR==2008|YEAR==2009)%>%
  select(c("SITE_ID","VISIT_NO","RT_WQI",
           "CL_pt","SO4_pt","PTL_pt","NTL_pt",
           "TURB_pt","ENTERO_PT",
           "DO_PT","PH_PT","NUMVARN","WQII"))

length(unique(wqii$SITE_ID)) #n=2123
#2008 2009
# 880 1581

# Water isotopes
isotope_org$DATE_COL<-as.Date(isotope_org$DATE_COL, format="%m/%d/%Y")
isotope_org$YEAR <-format(isotope_org$DATE_COL,"%Y")
table(isotope_org$YEAR)
#2008 2009
# 880 1581
isotope <- isotope_org%>%
  select(c("SITE_ID","VISIT_NO","SAMPLE_ID","dD..vsmow.","d18O..vsmow.","d.excess"))
length(unique(isotope$SITE_ID))#2063

# Check for duplicates n=613 (I think these must be lab duplicates for QAQC)
test<-isotope%>%
  group_by(SITE_ID)%>%
  filter(n()>1)

# WRITE REDUCED DATASETS TO NEW FOLDER TO MERGE
write.csv(site_org,"data_to_merge_0809/a_site.csv", row.names=FALSE)
write.csv(bent,"data_to_merge_0809/b_benth.csv", row.names=FALSE)
write.csv(benthic_oe,"data_to_merge_0809/c_benth_oe.csv", row.names=FALSE)
write.csv(chem,"data_to_merge_0809/c_chem.csv", row.names=FALSE)
#write.csv(land,"data_to_merge/d_land.csv", row.names=FALSE)
write.csv(phab,"data_to_merge_0809/e_phab.csv", row.names=FALSE)

write.csv(phab_oe,"data_to_merge_0809/f_phab_oe.csv", row.names=FALSE)
write.csv(wqii,"data_to_merge_0809/g_wqii.csv", row.names=FALSE)
write.csv(isotope,"data_to_merge_0809/h_isotope.csv", row.names=FALSE)

# Save in processed data folder
write.csv(strcat,"data_processed/subset_streamcat0809.csv", row.names=FALSE)

#########
## Use multi-merge function to bring data together
# Merging based on Site ID

# Need to treat SEMNRSA like a package to be able to run multimerge function
devtools::load_all()

nrsa_v1<- multimerge("data_to_merge_0809")
names(nrsa_v1)
length(unique(nrsa$SITE_ID)) #n=2069 out of 7018 (many are duplicates for lab and field measures)

# AGGREGATE ECOREGIONS
nrsa_v1$AG_ECO5<-nrsa_v1$AGGR_ECO9_2015
nrsa_v1<-nrsa_v1 %>%
  mutate(AG_ECO5 = case_when (
    (AGGR_ECO9_2015 %in% c('WMT','XER'))~'West',
    (AGGR_ECO9_2015 %in% c('NPL','SPL'))~'Great Plains',
    (AGGR_ECO9_2015 %in% c('UMW','TPL'))~'Midwest',
    (AGGR_ECO9_2015 %in% c('NAP','SAP'))~'Appalachians',
    (AGGR_ECO9_2015 %in% c('CPL'))~'Coastal Plains'))
table(nrsa_v1$AG_ECO5)

nrsa_v1$AG_ECO5<-ordered(nrsa_v1$AG_ECO5, levels=c("West", "Great Plains","Midwest",
                                             "Appalachians","Coastal Plains"))

#######################
## CHECK LISTS
# https://stackoverflow.com/questions/23353067/comparing-two-lists-r
length(unique(nrsa_v1$SITE_ID))
#create vectors combining siteID and visitno
org1<-site_org%>%
  unite('ID',SITE_ID,VISIT_NO)
org<-unlist(org1$ID) #n=2320

proc1<-nrsa_v1%>%
  unite('ID',SITE_ID,VISIT_NO)
proc<-unlist(proc1$ID)#

#See if in both
both<-org %in% proc
table(both) # n=2320

#What is in both
int <-intersect(org,proc)
int #2303

# What is different in original - nothing
difs <-setdiff(org, proc)
difs
# And what is different in processed - nothing
difs2 <-setdiff(proc,org)
difs2

###################
# Check for duplicate site-id and visit no n = 4915 out of 7018
duplic<-nrsa_v1%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)

####################
# Drop duplicate observations and retain first observation (even though isotope values are different)
# https://www.codecademy.com/courses/learn-dplyr/lessons/r-data-cleaning/exercises/duplicates#:~:text=To%20check%20for%20duplicates%2C%20we,which%20rows%20are%20duplicate%20rows.&text=We%20can%20see%20that%20the,calories%2C%20is%20a%20duplicate%20row.
# Retains first observation n = 2303 compared to n=7018
nrsa_v2= nrsa_v1%>%
  distinct(SITE_ID,VISIT_NO, .keep_all=TRUE)

#####################
# SUBSET ONLY VISIT_NO = 1 n = 2123 lakes
nrsa1_0809<-nrsa_v2 %>%
  filter(VISIT_NO==1)
length(unique(nrsa1_0809$SITE_ID)) # n = 2123

test=nrsa1_0809%>%
  group_by(SITE_ID)%>%
  filter(n()>1)

# Store processed data within the package
#usethis::use_data(nrsa1,overwrite=TRUE)
#usethis::use_data(nrsa_proc,overwrite=TRUE)

# EXPORE DATA AS .csv files
write.csv(nrsa_v2,"data_processed/nrsa0809_all.csv",row.names = FALSE)
write.csv(nrsa1_0809,"data_processed/nrsa0809_visit1.csv",row.names = FALSE)


###########################
## MERGE STREAMCAT VARS
# LOAD PROCESSED NRSA DATA n = 2261 obs
#nrsa<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/SEMNRSA/data_processed/nrsa0809_all.csv")
#nrsa1<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/SEMNRSA/data_processed/nrsa0809_visit1.csv")
#names(nrsa)

# Merge processed NRSA and StreamCat datasets together using left_join
nrsa_strmcatv1<-left_join(nrsa_v2,strcat,
                        by="SITE_ID")
# n = 2303 obs with 295 variables

#############
## DATA PROCESSING
# Need to match survey climate vars to year of survey (originally in land_org dataset)
# SUBSET DATA BY YEAR
first<-nrsa_strmcatv1%>%
  filter(YEAR=="2008")

second<-nrsa_strmcatv1%>%
  filter(YEAR=="2009")

# Create column of survey specific climate and water temp vars that can be joined together
# 2008
first<-first%>%
  mutate(
    MAST_SY=MAST_2008,
    MSST_SY=MSST_2008,
    MWST_SY=MWST_2008,
    PSUMPY_SY_PT=PSUMPY_2008_PT,
    PSUMPY_SY_WS=PSUMPY_2008Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2008_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2008Ws,
    TMEAN_WSY_PT=TMEAN_W_2008_PT,
    TMEAN_WSY_WS=TMEAN_W_2008Ws,
    TMEAN_SSY_PT=TMEAN_S_2008_PT,
    TMEAN_SSY_WS=TMEAN_S_2008Ws,
    TMEAN_SY_PT=TMEAN_SY_2008_PT,
    TMEAN_SY_WS=TMEAN_SY_2008Ws)%>%
  select(-c(MAST_2008,MSST_2008,MWST_2008,PSUMPY_2008_PT,PSUMPY_2008Ws,
            TMEAN_PW_2008_PT,TMEAN_PW_2008Ws,TMEAN_W_2008_PT,TMEAN_W_2008Ws,
            TMEAN_S_2008_PT,TMEAN_S_2008Ws,TMEAN_SY_2008_PT,TMEAN_SY_2008Ws,
            MAST_2009,MSST_2009,MWST_2009,PSUMPY_2009_PT,PSUMPY_2009Ws,
            TMEAN_PW_2009_PT,TMEAN_PW_2009Ws,TMEAN_W_2009_PT,TMEAN_W_2009Ws,
            TMEAN_S_2009_PT,TMEAN_S_2009Ws,TMEAN_SY_2009_PT,TMEAN_SY_2009Ws))
names(first)


# 2009
second<-second%>%
  mutate(
    MAST_SY=MAST_2009,
    MSST_SY=MSST_2009,
    MWST_SY=MWST_2009,
    PSUMPY_SY_PT=PSUMPY_2009_PT,
    PSUMPY_SY_WS=PSUMPY_2009Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2009_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2009Ws,
    TMEAN_WSY_PT=TMEAN_W_2009_PT,
    TMEAN_WSY_WS=TMEAN_W_2009Ws,
    TMEAN_SSY_PT=TMEAN_S_2009_PT,
    TMEAN_SSY_WS=TMEAN_S_2009Ws,
    TMEAN_SY_PT=TMEAN_SY_2009_PT,
    TMEAN_SY_WS=TMEAN_SY_2009Ws)%>%
  select(-c(MAST_2008,MSST_2008,MWST_2008,PSUMPY_2008_PT,PSUMPY_2008Ws,
            TMEAN_PW_2008_PT,TMEAN_PW_2008Ws,TMEAN_W_2008_PT,TMEAN_W_2008Ws,
            TMEAN_S_2008_PT,TMEAN_S_2008Ws,TMEAN_SY_2008_PT,TMEAN_SY_2008Ws,
            MAST_2009,MSST_2009,MWST_2009,PSUMPY_2009_PT,PSUMPY_2009Ws,
            TMEAN_PW_2009_PT,TMEAN_PW_2009Ws,TMEAN_W_2009_PT,TMEAN_W_2009Ws,
            TMEAN_S_2009_PT,TMEAN_S_2009Ws,TMEAN_SY_2009_PT,TMEAN_SY_2009Ws))
names(second)

## COMBINE SUBSETS TOGETHER USING ROWBIND
nrsa_strmcat_proc<-bind_rows(first,second)
#n = 2303 w/287 variables

# SUBSET ONLY VISIT_NO = 1 n = 2123 sites
nrsa_strmcat1<-nrsa_strmcat_proc %>%
  filter(VISIT_NO==1)
length(unique(nrsa_strmcat1$SITE_ID)) # n = 2123

# EXPORE DATA AS .csv files
write.csv(nrsa_strmcat_proc,"data_processed/nrsa0809/nrsa0809_strmcat_all.csv",row.names = FALSE)
write.csv(nrsa_strmcat1,"data_processed/nrsa0809/nrsa0809_strmcat_visit1.csv",row.names = FALSE)
#colnames(nrsa_strmcat1)
dat_names<-data.frame(colnames(nrsa_strmcat1))
write.csv(dat_names,"data_processed/nrsa0809/column_vars0809.csv",row.names = FALSE)



################################
## PROCESS DATA TO BE ABLE TO MERGE WITH OTHER SURVEYS
## READ PROCESSED DATA n = 2303
nrsa_strmcat_proc<-read.csv("data_processed/nrsa0809/nrsa0809_strmcat_all.csv")

# RENAME VARS TO MATCH OTHER SURVEYS
nrsa_strmcat_proc<-nrsa_strmcat_proc%>%
  rename("H2O_dD"="dD..vsmow.")%>%
  rename("H2O_d18O"="d18O..vsmow.")%>%
  rename("US_L3CODE" = "US_L3CODE_2015")%>%
  rename("US_L4CODE" = "US_L4CODE_2015")

# LANDCOVER/USE CLASSES
nrsa_strmcat_proc <-nrsa_strmcat_proc %>%
  mutate(PCTOW_WS=PctOw2008Ws,
         PCTICE_WS=PctIce2008Ws,
         PCTURBOP_WS=PctUrbOp2008Ws,
         PCTURBLO_WS=PctUrbLo2008Ws,
         PCTURBMD_WS=PctUrbMd2008Ws,
         PCTURBHI_WS=PctUrbHi2008Ws,
         PCTDECID_WS=PctDecid2008Ws,
         PCTCONIF_WS=PctConif2008Ws,
         PCTMXFST_WS=PctMxFst2008Ws,
         PCTSHRB_WS=PctShrb2008Ws,
         PCTGRS_WS=PctGrs2008Ws,
         PCTHAY_WS=PctHay2008Ws,
         PCTCROP_WS=PctCrop2008Ws,
         PCTWDWET_WS=PctWdWet2008Ws,
         PCTHBWET_WS=PctHbWet2008Ws,
         PCTOW_WsRp100=PctOw2008_WsRp100,
         PCTICE_WsRp100=PctIce2008_WsRp100,
         PCTURBOP_WsRp100=PctUrbOp2008_WsRp100,
         PCTURBLO_WsRp100=PctUrbLo2008_WsRp100,
         PCTURBMD_WsRp100=PctUrbMd2008_WsRp100,
         PCTURBHI_WsRp100=PctUrbHi2008_WsRp100,
         PCTDECID_WsRp100=PctDecid2008_WsRp100,
         PCTCONIF_WsRp100=PctConif2008_WsRp100,
         PCTMXFST_WsRp100=PctMxFst2008_WsRp100,
         PCTSHRB_WsRp100=PctShrb2008_WsRp100,
         PCTGRS_WsRp100=PctGrs2008_WsRp100,
         PCTHAY_WsRp100=PctHay2008_WsRp100,
         PCTCROP_WsRp100=PctCrop2008_WsRp100,
         PCTWDWET_WsRp100=PctWdWet2008_WsRp100,
         PCTHBWET_WsRp100=PctHbWet2008_WsRp100,
         PCTIMP_WS=PctImp2008Ws,
         PCTIMP_WsRp100=PctImp2008WsRp100)%>%
  select(-c(PctOw2008Ws, PctIce2008Ws, PctUrbOp2008Ws, PctUrbLo2008Ws, PctUrbMd2008Ws, PctUrbHi2008Ws,
            PctDecid2008Ws, PctConif2008Ws, PctMxFst2008Ws, PctShrb2008Ws, PctGrs2008Ws,
            PctHay2008Ws, PctCrop2008Ws, PctWdWet2008Ws, PctHbWet2008Ws,
            PctOw2008_WsRp100, PctIce2008_WsRp100, PctUrbOp2008_WsRp100,
            PctUrbLo2008_WsRp100, PctUrbMd2008_WsRp100, PctUrbHi2008_WsRp100,
            PctDecid2008_WsRp100, PctConif2008_WsRp100, PctMxFst2008_WsRp100,
            PctShrb2008_WsRp100, PctGrs2008_WsRp100, PctHay2008_WsRp100, PctCrop2008_WsRp100,
            PctWdWet2008_WsRp100, PctHbWet2008_WsRp100, PctImp2008Ws,
            PctImp2008WsRp100))

# CALCULATE XMW (woody riparian vegetation in understory)
# XCMGW - XC - XGW = XMW
nrsa_strmcat_proc<- nrsa_strmcat_proc%>%
  mutate(XMW = XCMGW - XGW - XC) %>%
  mutate(XMW = case_when(
    XMW<0 ~ 0,
    TRUE ~ XMW
  ))

# One observation was strange and not adding up - make into NA
nrsa_strmcat_proc$XMW[nrsa_strmcat_proc$SITE_ID=="FW08MT025" & nrsa_strmcat_proc$VISIT_NO==1]<- NA

summary(nrsa_strmcat_proc$XMW)

## REDUCE VARIABLES
nrsa0809<-nrsa_strmcat_proc%>%
  select(c("UID","SITE_ID","VISIT_NO","DATE_COL","YEAR","SITE_CLASS","STATE","AGGR_ECO3_2015","AGGR_ECO9_2015","AG_ECO5",
           "US_L3CODE","US_L4CODE",
           "LAT_DD83","LON_DD83","PROTOCOL","REALM","STRAHLERORDER",
           "MMI_BENT","OE_SCORE_OLD","OE_SCORE",
           "NH4","ANC","CL","COLOR","COND","DOC","MG","SODIUM","K","NO3","NO2","NTL","PTL","SO4","TSS","TURB",
           "RT_WQI","CL_pt","SO4_pt","PTL_pt","NTL_pt","TURB_pt","ENTERO_PT", "DO_PT","PH_PT","WQII",
           "H2O_dD","H2O_d18O","d.excess",
           "MAST_SY","MSST_SY","MWST_SY",
           "LDCBF_G08",
           "XDEPTH_CM","SDDEPTH_CM","XWXD","RP100","XBKF_W","XBKF_H","XINC_H","SINU","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SAFN","PCT_SFGF","LDMB_BW5","LRBS_BW5","LRBS_G08","PCT_FAST","PCT_SLOW",
           "RPRAT","PCT_BDRK","XFC_ALG","XFC_AQM","XFC_LWD","XFC_NAT","V1W_MSQ","XCDENBK","XCDENMID",
           "XCL","XGB","XGW","XMW","XC","XCMGW","QR1","QRVEG1","RDIST1","W1_HALL","W1H_WALL","W1_HNOAG","W1_HAG",
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

#n = 2123 with 184 variables
#######################
#GET COLUMN NAMES TO CHECK
dat_names_share<-data.frame(colnames(nrsa0809))
write.csv(dat_names_share,"data_processed/nrsa0809/column_varsSHARE_0809.csv", row.names=FALSE)

## WRITE TO CSV
write.csv(nrsa0809,"data_processed/nrsa0809/nrsa0809_to_merge.csv", row.names=FALSE)

