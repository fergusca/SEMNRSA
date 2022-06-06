######################
## CONFIRMATORY FACTOR ANALYSIS
##  Examine water quality constructs
#
# 4/12/2022
#####################

rm(list=ls())

library(lavaan)

################
## LOAD DATA
# PROCESSED - all 0809 and only new sites from later surveys
#  VISITS 1 and 2 n = 4578
dat_org <- read.csv("data_processed/Compiled/nrsa081318_nonresampled_VISIT_12.csv")

# PROCESSED DATA VISIT_NO=1 ONLY n = 4389
dat<-dat_org%>%
  filter(VISIT_NO==1)

# Transform variables
dat$L_SODIUM <-log10(dat$SODIUM_RESULT)
#dat$L_NABD_NrmStorWs_ratio <- log10(dat$NABD_NrmStorWs_ratio+0.001)

#dat$asin_PCTAGR_WS<-asin(sqrt(dat$PCTAGR_WS/100))
#dat$asin_PCTURB_WS<-asin(sqrt(dat$PCTURB_WS/100))
#dat$asin_PCTFOR_WsRp100<-asin(sqrt(dat$PCTFOR_WsRp100/100))
#dat$asin_PCTWET_WsRp100<-asin(sqrt(dat$PCTWET_WsRp100/100))

summary(dat$asin_PCTAGR_WS)

################
## NAP DATA SUBSET n = 486
nap<-dat%>%
  filter(AG_ECO9=="NAP")%>%
  drop_na(c(LOE_QLow_cl,L_NABD_NrmStorWs_ratio))

summary(nap$LOE_Qbkf_cl)
summary(nap$L_NABD_NrmStorWs_ratio)

# SCALE CUMULATIVE PRECIPITATION in NAP
nap$PSUMPY_SY_WS_sc<-scale(nap$PSUMPY_SY_WS)
summary(nap$PSUMPY_SY_WS_sc)

# WADEABLE n = 280
nap_w <- nap %>%
  filter(PROTOCOL=="WADEABLE")

# BOATABLE n = 206
nap_b <-nap %>%
  filter(PROTOCOL=="BOATABLE")

###########################
## CONFIRMATORY FACTOR ANALYSIS
# https://stats.oarc.ucla.edu/r/seminars/rcfa/

m1a <- 'nutrient =~ L_PTL + L_NTL + L_TURB
        ion =~ L_CHLR + L_SULF #+ L_SODIUM
        L_PTL ~~ L_TURB
        '
twofac3items_a <- cfa(m1a, data=nap_w, std.lv=FALSE)
summary(twofac3items_a,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)

mi_min <-modindices(twofac3items_a)
print(mi_min[mi_min$mi >3.0,])

#############
# Export R output - REVISED MODEL 7
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_CFA<- capture.output(summary(twofac3items_a, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_CFA, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/NAPw_CFA_wq.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(twofac3items_a)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/NAPw_CFA_wq.csv",
          row.names = FALSE)


# LOOK AT CFA USING DIAGRAM PACKAGES
cfa_wq<- lavaanPlot(model=twofac3items_a, node_options=list(shape="box",fontname= "Helvetica"),
                edge_options = list(color="grey"), coefs=TRUE, covs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(cfa_wq,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/CFA_wq_NAPw.png")

###########################
# Things to look for in output
# https://www.youtube.com/watch?v=eUwaNcnJLfA
# See if Variances are positive, R2 are less than 1, - Heywood case if variances are negative adn R2 are 1
# Also check standard error values and see if they are approximately the same for each item?

# Look at standardized all to see if parameters make sense
# Don't want standard errors to be zero - because no variance to predict

# Can look at the residuals between the estimated covariance matrix and the actual matrix
#  ones with large residuals indicate that we aren't doing a great job estimating that relationships
# this one looks pretty good I think
residuals(twofac3items_a)

################
# SEM predicting water quality
wq_m1 <- '
# Latent vars
  nutrient =~ L_PTL + L_NTL + L_TURB
  ion =~ L_CHLR + L_SULF #+ L_SODIUM
# Structural model
LQLow_cl ~ asin_PCTAGR_WS + asin_PCTURB_WS + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100   #+ NABD_NIDStorWs ##
LQbkf_cl ~ asin_PCTAGR_WS + asin_PCTURB_WS + L_NABD_NrmStorWs_ratio + asin_PCTFOR_WsRp100
Lpt01_XCMGW ~ asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + W1_HAG + W1_HNOAG #+ asin_PCTWET_WsRp100
nutrient ~ asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + LQLow_cl + LQbkf_cl + Lpt01_XCMGW #+ asin_PCTWET_WsRp100
ion ~ asin_PCTAGR_WS + asin_PCTURB_WS + asin_PCTFOR_WsRp100 + LQLow_cl + LQbkf_cl + Lpt01_XCMGW + L_NABD_NrmStorWs_ratio

# Covariances
L_PTL ~~ L_TURB
LQLow_cl~~LQbkf_cl
asin_PCTAGR_WS~~W1_HAG
asin_PCTURB_WS~~W1_HNOAG
#asin_PCTAGR_WS~~asin_PCTFOR_WsRp100
#asin_PCTURB_WS~~asin_PCTFOR_WsRp100
'

# Fit model
fit_wq_m1<- sem(wq_m1, data=nap_w,
                       missing="ML", se="bootstrap")

summary(fit_wq_m1, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_wq_m1)
print(mi_min[mi_min$mi >3.0,])

#####################
## SAVE SEM OUTPUT
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_fit_SEM<- capture.output(summary(fit_wq_m1, standardized=TRUE, fit.measures=TRUE,rsquare=TRUE)) #, modindices=T
write.csv(out_fit_SEM, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/NAPw_SEM_wq.csv",
          row.names=FALSE)

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_wq_m1)
write.csv(std_parameter_se_bootstrap_min, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/NAPw_SEM_wq.csv",
          row.names = FALSE)
##############
## PRINT GRAPH
sem_wq<- lavaanPlot(model=fit_wq_m1, node_options=list(shape="box",fontname= "Helvetica"),
                    edge_options = list(color="grey"), coefs=TRUE, stand=TRUE, sig=0.05,stars=c("regress")) #covs=TRUE,

save_png(sem_wq,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_OE_project/Project_repository/Routput/SEM_output/SEM_wq_NAPw.png")



mymodel_v6 <- '
LOE_XCMGW_use ~ PCTAGR_WS + PCTURB_WS + W1_HAG + W1_HNOAG + PCTFOR_WsRp100 + PCTWET_WsRp100
LOE_QLow_cl ~ PCTAGR_WS + PCTURB_WS + PSUMPY_SY_WS_sc  #+ NABD_NIDStorWs ##
LOE_Qbkf_cl ~ PCTAGR_WS + PCTURB_WS + PSUMPY_SY_WS_sc  #+ NABD_NIDStorWs #NABD_NIDStorWs_ratio #
d.excess ~  LOE_QLow_cl + LOE_Qbkf_cl + PSUMPY_SY_WS_sc  #+ NABD_NIDStorWs # NABD_NIDStorWs_ratio #
LOE_RBS_use ~ PCTAGR_WS + PCTURB_WS + LOE_XCMGW_use + LOE_QLow_cl + LOE_Qbkf_cl + W1_HAG + W1_HNOAG + PCTFOR_WsRp100 + PCTWET_WsRp100
LOE_XFC_NAT_use ~ PCTAGR_WS + PCTURB_WS + LOE_XCMGW_use + LOE_QLow_cl + LOE_Qbkf_cl + W1_HAG + W1_HNOAG + PCTFOR_WsRp100 + PCTWET_WsRp100
WQII ~ PCTAGR_WS + PCTURB_WS + LOE_XCMGW_use + d.excess +  W1_HAG + W1_HNOAG + PCTFOR_WsRp100 + PCTWET_WsRp100 #+ LOE_QLow_cl + LOE_Qbkf_cl
OE_SCORE ~ LOE_XCMGW_use + LOE_RBS_use + LOE_XFC_NAT_use + WQII + LOE_Qbkf_cl + LOE_QLow_cl + PCTAGR_WS + PCTURB_WS + PCTFOR_WsRp100 + PCTWET_WsRp100

# Covariance
LOE_QLow_cl~~LOE_Qbkf_cl
PCTAGR_WS~~PCTFOR_WsRp100
PCTURB_WS~~PCTFOR_WsRp100
PCTAGR_WS~~W1_HAG
PCTURB_WS~~W1_HNOAG
PCTFOR_WsRp100~~W1_HAG
PCTFOR_WsRp100~~W1_HNOAG
'
fit_v6_NAP_wade <- sem(mymodel_v6, data=nap_w,
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #auto.var=TRUE (will estimate residual variances), group="Lake_Origin_mod")

summary(fit_v6_NAP_wade, standardized=TRUE, fit.measures=TRUE, modindices=F,rsquare=TRUE)#, modindices=T, rsquare=TRUE) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_v6_NAP_wade)
print(mi_min[mi_min$mi >3.0,])




# Fixing and freeing parameters
m1b <- 'wqi =~ NA*PTL_pt + NTL_pt + TURB_pt
        wqi ~~ 1*wqi ' #NA* will free the loading parameter on the first item which be default fixes it to one and 1* will fix variance of factor to one

onefac3items_b <- cfa(m1b, data=dat)
summary(onefac3items_b, fit.measures=TRUE, standardized=TRUE)



####################
## GROUP INVARIANCE TESTING WITH VARIOUS COMPONENTS OF CFA
## BY AGGREGATED ECOREGION 3
## https://www.youtube.com/watch?v=t6uHROGalBo
table(dat$AG_ECO3)
#EHIGH PLNLOW  WMTNS
#550   1013    506
#Appalachians Coastal Plains   Great Plains        Midwest           West
#       550            236            336            441            506

overall.model <- '
  wqi =~  PTL_pt + NTL_pt + TURB_pt'

overall.fit <- cfa(model=overall.model,
                   data=dat,
                   meanstructure=TRUE)
summary(overall.fit,
        standardized=TRUE,
        rsquare=TRUE,
        fit.measure=TRUE)

# TABLE OF FIT INDICES
table_fit <- matrix(NA, nrow=9, ncol=6)
colnames(table_fit) = c("Model","X2", "df","CFI", "RMSEA","SRMR")
table_fit[1,] <- c("Overall Model", round(fitmeasures(overall.fit,
                                                      c("chisq","df","cfi",
                                                        "rmsea","srmr")),3))
kable(table_fit)

###########################
## CONFIGURAL INVARIANCE
# Letting everything vary among groups
config_invar.fit<- cfa(model=overall.model,
                       data=dat,
                       meanstructure=TRUE,
                       group="AG_ECO3")
summary(config_invar.fit,
        standardized=TRUE,
        rsquare=TRUE,
        fit.measure=TRUE)
table_fit[2,] <- c("Config invar", round(fitmeasures(config_invar.fit,
                                                     c("chisq","df","cfi",
                                                       "rmsea","srmr")),3))
#####################
# METRIC INVARIANCE - CONSTRAINS FACTOR LOADINGS
# ARE THE FACTOR LOADINGS THE SAME ACROSS GROUPS? - adds letters by the loadings so that can specify to be the same across the groups
metric.fit<- cfa(model=overall.model,
                 data=dat,
                 meanstructure=TRUE,
                 group="AG_ECO3",
                 group.equal=c("loadings"))

summary(metric.fit,
        standardized=TRUE,
        rsquare=TRUE,
        fit.measure=TRUE)

table_fit[3,] <- c("Metric Model", round(fitmeasures(metric.fit,
                                                     c("chisq","df","cfi",
                                                       "rmsea","srmr")),3))
kable(table_fit)

######################
# SCALAR INVARIANCE
# ARE THE ITEM INTERCEPTS THE SAME? - DO THEY START IN THE SAME PLACE?
# SLOPES MAY BE THE SAME BUT STARTING POINTS MAY BE DIFFERENT
scalar.fit<- cfa(model=overall.model,
                 data=dat,
                 meanstructure=TRUE,
                 group="AG_ECO3",
                 group.equal=c("loadings","intercepts"))

summary(scalar.fit,
        standardized=TRUE,
        rsquare=TRUE,
        fit.measure=TRUE)

table_fit[4,] <- c("Scalar Model", round(fitmeasures(scalar.fit,
                                                     c("chisq","df","cfi",
                                                       "rmsea","srmr")),3))
kable(table_fit)

###############################
# STRICT ERROR INVARIANCE
# ARE THE ITEM RESIDUALS THE SAME? Is the variance around the item the same between groups?
error.fit<-cfa(model = overall.model,
               data=dat,
               meanstructure=TRUE,
               group="AG_ECO3",
               group.equal=c("loadings","intercepts","residuals"))


summary(error.fit,
        standardized=TRUE,
        rsquare=TRUE,
        fit.measure=TRUE)

table_fit[5,] <- c("Strict Error Model", round(fitmeasures(error.fit,
                                                           c("chisq","df","cfi",
                                                             "rmsea","srmr")),3))
kable(table_fit)


###########################
## PARTIAL INVARIANCE _ ALLOWING FOR SLOPES TO VARY
partial_syntax <- paste("wqi",
                        "=~",
                        colnames(dat)[c(210,212:214)]) # columns
CFI_list <- 1:length(partial_syntax)
names(CFI_list)<- partial_syntax

for(i in 1:length(partial_syntax)){
  temp<- cfa(model = overall.model,
             data=dat,
             meanstructure=TRUE,
             group="AG_ECO3",
             group.equal=c("loadings"),
             group.partial=partial_syntax[i])

  CFI_list[i] <-fitmeasures(temp, "cfi")

}
CFI_list

# COMPARE CFI values
options(scipen = 999)
sort(CFI_list - fitmeasures(metric.fit, "cfi"), decreasing = T)

#wqi =~ TURB_pt           wqi =~ PTL_pt            wqi =~ CL_pt
#0.01250767750334313977  0.00963773302103121399 -0.00000000000002819966
#wqi =~ NTL_pt
#-0.00066590772217656191

###########################
# NOW REVISE MODEL ALLOWING FOR TURBIDITY TO VARY
metric.fit_2<- cfa(model=overall.model,
                   data=dat,
                   meanstructure=TRUE,
                   group="AG_ECO3",
                   group.equal=c("loadings"),
                   group.partial=c("wqi =~ TURB_pt"))

summary(metric.fit_2,
        standardized=TRUE,
        rsquare=TRUE,
        fit.measure=TRUE)

table_fit[6,] <- c("Metric Model_turb", round(fitmeasures(metric.fit_2,
                                                          c("chisq","df","cfi",
                                                            "rmsea","srmr")),3))
kable(table_fit)



###########################
## PARTIAL INVARIANCE _ ALLOWING FOR Intercepts TO VARY
partial_syntax <- paste(colnames(dat)[c(212:214)],
                        "~",
                        1) # columns
CFI_list <- 1:length(partial_syntax)
names(CFI_list)<- partial_syntax

for(i in 1:length(partial_syntax)){
  temp<- cfa(model = overall.model,
             data=dat,
             meanstructure=TRUE,
             group="AG_ECO3",
             group.equal=c("loadings","intercepts"),
             group.partial=partial_syntax[i])

  CFI_list[i] <-fitmeasures(temp, "cfi")

}
CFI_list

# COMPARE CFI values
options(scipen = 999)
sort(CFI_list - fitmeasures(scalar.fit, "cfi"), decreasing = T)


