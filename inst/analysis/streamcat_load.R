# READ STREAMCAT DATA EXPORT

library(dplyr)
library(ggplot2)

# To plot PCA
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

# On OneDrive
streamcat <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/StreamCat/watershed_2022_0110.csv")

#wshed <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/StreamCat/watershed_NLCD2011_2022_0111.csv")
#rpcatch<-

# LOAD PROCESSED NRSA DATA n = 2261 obs
nrsa<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/SEMNRSA/data_processed/nrsa_all.csv")
nrsa1<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/SEMNRSA/data_processed/nrsa_visit1.csv")
names(nrsa)

# see if NRSA missing COMID
miss<-nrsa1 %>%
  filter(is.na(COMID))

#df=2069 obs
ids<-nrsa1%>%
  select(COMID)

# Select observations with COMIDS in NRSA
test<-streamcat%>%
  filter(COMID %in% ids$COMID)
# selects 2039 out of 2069

