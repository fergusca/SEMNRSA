#########################
## DATA PROCESSING TO JOIN PHDI DATA
##
## 5/25/2022

library(dplyr)
library(tidyr)

#######################
## ADD PHDI DATA - MARC EMAILED PROCESSED DATA 5/23/2022 & 5/25/22
# Read in relevant data sets
## FULL NRSA WITH VISITS 1 and 2 n = 6674
nrsa<- read.csv("data_processed/Compiled/NRSA_081318_all.csv")

# PHDI dataset n=13076 - Marc added nine sites that were missing PHDI values from earlier rendition
phdi<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/Climate/nrsa_phdi_allyearsmonths_v3.csv")
# PHDI dataset - all nrsa sites (prob and extent) - note there are many duplicated UNIQUE_IDs in this dataset
#phdi<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/Climate/nrsa_phdi_allyearsmonths_ALL_PROB.csv")
# Reduced nrsa sites PROB and CORE n = 5633
#phdi<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/Climate/nrsa_phdi_allyearsmonths.csv")

# SUBSET NRSA data to include UNIQUE_ID, SITE_ID, DATE_COL, YEAR
nrsa_red <- nrsa%>%
  select("UNIQUE_ID","DATE_COL","YEAR")

# Remove duplicated UNIQUE_ID from phdi dataset from 13076 to n = 4389
test<- phdi %>%
  distinct(UNIQUE_ID, .keep_all = TRUE)

# MERGE WITH PHDI n = 6674
dat <-left_join(nrsa_red, test,
                by="UNIQUE_ID")

################
# CREATE MONTH AND YEAR NRSA SAMPLE Date column to select for PHDI month and year
# https://www.statmethods.net/input/dates.html#:~:text=You%20can%20use%20the%20as,format%20gives%20the%20appropriate%20format.
# FORMAT DATE
dat$DATE_COL<-as.Date(dat$DATE_COL, format="%Y-%m-%d")
str(dat$DATE_COL)
dat$month_yr <-format(dat$DATE_COL,"%Y-%m")
str(dat$month_yr)
tail(dat$month_yr)

########
## GRAB PHDI FOR MONTH AND YEAR OF SAMPLE
# FILL IN NEW COLUMN FOR PHDI DURING SAMPLE MONTH
dat_phdi_m <- dat%>%
  mutate(phdi_month = case_when(
    month_yr=="2008-06" ~ X8.Jun,
    month_yr=="2008-07" ~ X8.Jul,
    month_yr=="2008-08" ~ X8.Aug,
    month_yr=="2008-09" ~ X8.Sep,
    month_yr=="2008-10" ~ X8.Oct,
    month_yr=="2008-11" ~ X8.Nov,
    month_yr=="2008-12" ~ X8.Dec,
    month_yr=="2009-04" ~ X9.Apr,
    month_yr=="2009-05" ~ X9.May,
    month_yr=="2009-06" ~ X9.Jun,
    month_yr=="2009-07" ~ X9.Jul,
    month_yr=="2009-08" ~ X9.Aug,
    month_yr=="2009-09" ~ X9.Sep,
    month_yr=="2009-10" ~ X9.Oct,
    month_yr=="2009-11" ~ X9.Nov,
    month_yr=="2013-04" ~ X13.Apr,
    month_yr=="2013-05" ~ X13.May,
    month_yr=="2013-06" ~ X13.Jun,
    month_yr=="2013-07" ~ X13.Jul,
    month_yr=="2013-08" ~ X13.Aug,
    month_yr=="2013-09" ~ X13.Sep,
    month_yr=="2013-10" ~ X13.Oct,
    month_yr=="2014-04" ~ X14.Apr,
    month_yr=="2014-05" ~ X14.May,
    month_yr=="2014-06" ~ X14.Jun,
    month_yr=="2014-07" ~ X14.Jul,
    month_yr=="2014-08" ~ X14.Aug,
    month_yr=="2014-09" ~ X14.Sep,
    month_yr=="2014-10" ~ X14.Oct,
    month_yr=="2018-05" ~ X18.May,
    month_yr=="2018-06" ~ X18.Jun,
    month_yr=="2018-07" ~ X18.Jul,
    month_yr=="2018-08" ~ X18.Aug,
    month_yr=="2018-09" ~ X18.Sep,
    month_yr=="2018-10" ~ X18.Oct,
    month_yr=="2019-04" ~ X19.Apr,
    month_yr=="2019-05" ~ X19.May,
    month_yr=="2019-06" ~ X19.Jun,
    month_yr=="2019-07" ~ X19.Jul,
    month_yr=="2019-08" ~ X19.Aug,
    month_yr=="2019-09" ~ X19.Sep,
    month_yr=="2019-10" ~ X19.Oct))


summary(dat_phdi_m$phdi_month)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#-8.1100 -1.2700  1.4200  0.9583  2.7900  9.8900       3
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -8.110  -1.280   1.420   0.961   2.800   9.890     486
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -8.1100 -1.2700  1.4200  0.9597  2.7900  9.8900      22

##################
# CALCULATE ANNUAL MEAN PHDI BY SITE DURING THE SURVEY YEAR
# # https://stackoverflow.com/questions/33401788/dplyr-using-mutate-like-rowmeans
##################
# SUBSET BY YEAR AND CALCULATE MEAN
# 2008
dat08<- dat_phdi_m%>%
  filter(YEAR=="2008")#%>%
#select(c(UNIQUE_ID, DATE_COL, YEAR, SITE_ID, X8.Jan:X8.Dec))
head(dat08)

dat08_long<- dat08%>%
  gather(Time,PHDI,X8.Jan:X8.Dec,-UNIQUE_ID)

# Calculate annual mean and make wide format again
dat08_mean<-dat08_long%>%
  group_by(UNIQUE_ID)%>%
  mutate(phdi_mean = mean(PHDI))%>%
  spread(Time,PHDI) # make wide format again

summary(dat08_mean$phdi_mean)

#################
# 2009 - compiled steps in one place
dat09<- dat_phdi_m%>%
  filter(YEAR=="2009")%>%
  gather(Time,PHDI,X9.Jan:X9.Dec,-UNIQUE_ID)%>%
  group_by(UNIQUE_ID)%>%
  mutate(phdi_mean = mean(PHDI))%>%
  spread(Time,PHDI) # make wide format again

summary(dat09$phdi_mean)
head(dat09$phdi_mean)

#################
# 2013 - compiled steps in one place
dat13<- dat_phdi_m%>%
  filter(YEAR=="2013")%>%
  gather(Time,PHDI,X13.Jan:X13.Dec,-UNIQUE_ID)%>%
  group_by(UNIQUE_ID)%>%
  mutate(phdi_mean = mean(PHDI))%>%
  spread(Time,PHDI) # make wide format again

summary(dat13$phdi_mean)
head(dat13$phdi_mean)

#################
# 2014 -
dat14<- dat_phdi_m%>%
  filter(YEAR=="2014")%>%
  gather(Time,PHDI,X14.Jan:X14.Dec,-UNIQUE_ID)%>%
  group_by(UNIQUE_ID)%>%
  mutate(phdi_mean = mean(PHDI))%>%
  spread(Time,PHDI) # make wide format again

summary(dat14$phdi_mean)
head(dat14$phdi_mean)

#################
# 2018
dat18<- dat_phdi_m%>%
  filter(YEAR=="2018")%>%
  gather(Time,PHDI,X18.Jan:X18.Dec,-UNIQUE_ID)%>%
  group_by(UNIQUE_ID)%>%
  mutate(phdi_mean = mean(PHDI))%>%
  spread(Time,PHDI) # make wide format again

summary(dat18$phdi_mean)
head(dat18$phdi_mean)

#################
# 2019
dat19<- dat_phdi_m%>%
  filter(YEAR=="2019")%>%
  gather(Time,PHDI,X19.Jan:X19.Dec,-UNIQUE_ID)%>%
  group_by(UNIQUE_ID)%>%
  mutate(phdi_mean = mean(PHDI))%>%
  spread(Time,PHDI) # make wide format again

summary(dat19$phdi_mean)
head(dat19$phdi_mean)

############################
## BRING DATASETS TOGETHER bind_rows n = 6674
dat_mean <- bind_rows(dat08_mean, dat09,dat13,dat14,dat18,dat19)%>%
  select(UNIQUE_ID,DATE_COL,phdi_mean)

summary(dat_mean$phdi_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#-7.3092 -0.7225  0.7963  0.8170  2.4075  8.9017      22

# JOIN PHDI VARIABLES TOGETHER n = 6674 - GOOD!
dat_phdi_m<- dat_phdi_m%>%
  select(UNIQUE_ID,DATE_COL,phdi_month)

dat_mod_phdi <-left_join(dat_phdi_m,dat_mean, by=c("UNIQUE_ID","DATE_COL"))

# FORMAT DATE in NRSA
nrsa$DATE_COL<-as.Date(nrsa$DATE_COL, format="%Y-%m-%d")
str(nrsa$DATE_COL)

# MERGE WITH PROCESSED NRSA DATA n = 6674
dat_nrsa_phdi <-left_join(nrsa, dat_mod_phdi,
                          by=c("UNIQUE_ID","DATE_COL"))

names(dat_nrsa_phdi)
# CHECK MEAN PHDI AND MONTH PHDI
nrsa_sites <- c("NRS_AL-10005","NRS_CA-11212",
                "NRS_CO-10500","NRS_GA-10086",
                "NRS_KS-10324","NRS_MI-10098",
                "NRS_MT-10296","NRS_NE-10157",
                "NRS_NV-10480","NRS_OK-10050",
                "NRS_RI-10048","NRS_SD-10400",
                "NRS_TX-10175","NRS_WA-10206",
                "NRS_WI-10212","NRS_WY-106042")
check<-dat_nrsa_phdi%>%
  select(UNIQUE_ID,YEAR,DATE_COL,phdi_mean,phdi_month)%>%
  filter(UNIQUE_ID %in% nrsa_sites)
#filter(UNIQUE_ID=="NRS_AL-10005" |UNIQUE_ID=="NRS_CO-10494")

summary(dat_nrsa_phdi$phdi_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#-7.3092 -0.7225  0.7958  0.8158  2.4075  8.9017       3

####################################
# LOOK AT DATA WITH MISSING PHDI - only three observations - all don't have UNIQUE_ID
missing<-dat_nrsa_phdi%>%
  filter(is.na(phdi_mean)) %>%
  select(UNIQUE_ID,SITE_ID,DATE_COL, phdi_mean,phdi_month)
length(unique(missing$UNIQUE_ID))
miss_nrsa<-missing%>%
  distinct(UNIQUE_ID, .keep_all = T)%>%
  select(UNIQUE_ID,SITE_ID)

missing_id<-missing$UNIQUE_ID

phdi_miss<-phdi%>%
  filter(UNIQUE_ID%in%missing_id)
length(unique(phdi_miss$UNIQUE_ID))
length(unique(phdi_miss$LON_DD83))

# EXPORT FILE OF MISSING PHDI FOR MARC
#write.csv(phdi_miss,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Data/Climate/missing_phdi_2022_0525.csv",
#          row.names=FALSE)

#########################
## WRITE PHDI FILES

write.csv(dat_mod_phdi,"data_processed/Compiled/phdi_mean_month.csv", row.names=FALSE)


##################
## OLD SCRIPT TYRING NOT TO SUBSET BY YEAR
# MAKE DATASET LONG USING GATHER FUNCTION
test2<-dat %>% gather(Time, PHDI, X8.Jan:X19.Dec,-UNIQUE_ID)
head(test2)
# Create new columns for month and year using stringR
test3<- test2%>%
  mutate(phdi_yr = str_extract(Time, "\\d+"))#%>%
#mutate(phdi_mon = str_sub(Time,-3))
table(test3$phdi_yr)
#table(test3$phdi_mon)

# CALCULATE MEAN PHDI FOR SURVEY YEAR n = 4390 out of 6674
test4<- test3%>%
  group_by(phdi_yr,UNIQUE_ID)%>%
  mutate(phdi_mean=mean(PHDI))%>%
  ungroup()%>%
  spread(Time, PHDI)%>%
  select(UNIQUE_ID,YEAR,SITE_ID,phdi_mean)%>%
  distinct(UNIQUE_ID, .keep_all=TRUE)
