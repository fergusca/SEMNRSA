#################
# GET OMERNIK ECOREGIONS

# NRSA 1314
site_org <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_1213_website/nrsa1314_siteinformation_wide_04292019.csv")

# NRSA 0809
site_0809<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/NRSA_0809_website/siteinfo_0.csv")

# SELECT DISTINCT US_L3CODES n = 84
l3<-site_org%>%
  distinct(US_L3CODE, .keep_all=TRUE)%>%
  select(US_L3CODE, US_L3NAME)
l4<-site_org%>% # n=577
  distinct(US_L4CODE, .keep_all=TRUE)%>%
  select(US_L4CODE, US_L4NAME)

l3_0809<- site_0809%>%
  distinct(US_L3CODE_2015, .keep_all=TRUE)%>%
  select(US_L3CODE_2015)
summary(l3_0809$US_L3CODE_2015)

#######################
## TRY POPULATING NAMES BY MERGING COLUMNS TOGETHER
test<-left_join(site_0809, l3,
                by=c("US_L3CODE_2015" = "US_L3CODE"))
