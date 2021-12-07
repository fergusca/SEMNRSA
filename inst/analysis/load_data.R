################
## LOAD NRSA DATA
###########
# LOAD DATA - NRSA 2012-13 from website

# Libraries
library(dplyr)

library(SEMNRSA)

# LOAD data stored in Rpackage - a .rda file
#load(file="data/nrsa.rda")

# ORIGINAL NRSA 2013-14 DATA downloaded from website 8/16/21
site_org<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_siteinformation_wide_04292019.csv")
chem_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_widechem_04232019.csv")
bent_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_bentmmi_04232019.csv")
land_org <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_landmet_02132019.csv")

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
  select(c("SITE_ID","VISIT_NO","MMI_BENT","OE_SCORE"))

land<-land_org%>%
  select(-c("PUBLICATION_DATE","UID","DATE_COL",
            "SITETYPE","INDEX_VISIT",'LAT_DD83',"LON_DD83"))

# WRITE REDUCED DATASETS TO NEW FOLDER TO MERGE
write.csv(site_org,"processed_data/a_site.csv")
write.csv(bent,"processed_data/b_benth.csv")
write.csv(chem,"processed_data/c_chem.csv")
write.csv(land,"processed_data/d_land.csv")

#########
## Use multi-merge function to bring data together
# Merging based on Site ID
nrsa<- multimerge("processed_data")
names(nrsa)
nrsa<-nrsa %>%
  select(-c("X.x","X.y",'X.x',"X.y"))

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

# SUBSET ONLY VISIT_NO = 1 n = 2069 lakes
nrsa1<-nrsa %>%
  filter(VISIT_NO==1)
length(unique(nrsa1$SITE_ID)) # n = 2069

# Store processed data within the package
usethis::use_data(nrsa1)

usethis::use_data(nrsa)


