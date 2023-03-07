#' Process SEM effects output for OE,MMI, EPT relabel variable names to make into a table
#'
#' @param sem_output SEM estimated effects output saved as .csv file
#'
#' @return Processed dataframe that selected direct, indirect, and total effects
#' @export
#'
#' @examples
#'west_oe_m1_full<- sem_eff_tab("model1") # name dataframe loaded with raw SEM effects output
sem_eff_tab = function(sem_output){
  sem_out_proc <- sem_output %>%
    filter(op==":="|op=="~"&lhs=="OE_SCORE"|op=="~"&lhs=="MMI_BENT_sc"|
             op=="~"&lhs=="EPT_RICH_sc")%>%
    select(lhs,label,est.std,se, pvalue,ci.lower, ci.upper)%>%
    rename(Predictor = label)%>%
    mutate(across(where(is.numeric), round,2))%>%
    mutate(lhs = case_when(str_detect(lhs,"OE_SCORE")~"Direct",
                           str_detect(lhs, "MMI_BENT_sc")~"Direct",
                           str_detect(lhs,"EPT_RICH_sc")~"Direct",
                           str_detect(lhs,"_oe") ~ "Indirect",
                           str_detect(lhs,"_mmi") ~ "Indirect",
                           str_detect(lhs,"_ept") ~ "Indirect",
                           str_detect(lhs,"_tot") ~ "Total",
                           str_detect(lhs, "_xcmgwtot") ~ "Total XCMGW",
                           str_detect(lhs,"_tntot") ~ "Total TN",
                           str_detect(lhs,"_rbstot") ~ "Total RBS",
                           str_detect(lhs,"_sulftot") ~ "Total SO4",
                           str_detect(lhs,"_evtot") ~ "Total Evap",
                           str_detect(lhs,"_lftot") ~ "Total Low flow",
                           str_detect(lhs,"_bftot") ~ "Total Bankfull flow",
                           str_detect(lhs,"_sptot") ~ "Total Specific power"))%>%
    mutate(Predictor = recode_factor(Predictor,
                                     a1="Agr Ws",u1="Developed Ws",sd1="Slope*depth",r1="Bed stability",sp1="Stream power",
                                     f1="Forest Rp",w1="Wetland Rp", rn1="Terrestrial cover Rp",l1="Summer flow",b1="Bankfull flow",x1="Riparian site index",xn1="Instream cover",
                                     na1="Non-agr index Rp", p1="Precipitation",t1="Max temp", ph1="Drought index",
                                     n1="TN",su1="Sulfate", tu1="Turbidity",
                                     agr_oe="Agr Ws",urb_oe="Developed Ws",
                                     rfor_oe="Forest Rp",
                                     rwet_oe="Wetland Rp",
                                     rnat_oe="Terrestrial cover Rp",
                                     dam_oe="Dam",slope_oe="Slope*depth",strpwr_oe="Stream power",
                                     hag_oe="Agr index Rp",nhag_oe="Non-agr index Rp",
                                     precip_oe="Precipitation",tmax_oe="Max temp",
                                     phdi_oe="Drought index",
                                     dexcess_oe="Evaporation indicator",lflow_oe="Summer flow",
                                     bflow_oe="Bankfull flow",xcmgw_oe="Riparian site index",
                                     agr_mmi="Agr Ws",urb_mmi="Developed Ws",
                                     rfor_mmi="Forest Rp",
                                     rwet_mmi="Wetland Rp",
                                     rnat_mmi="Terrestrial cover Rp",
                                     dam_mmi="Dam",slope_mmi="Slope*depth",strpwr_mmi="Stream power",
                                     hag_mmi="Agr index Rp",nhag_mmi="Non-agr index Rp",
                                     precip_mmi="Precipitation",tmax_mmi="Max temp",
                                     phdi_mmi="Drought index",
                                     dexcess_mmi="Evaporation indicator",lflow_mmi="Summer flow",
                                     bflow_mmi="Bankfull flow",xcmgw_mmi="Riparian site index",
                                     agr_ept="Agr Ws",urb_ept="Developed Ws",
                                     rfor_ept="Forest Rp",
                                     rwet_ept="Wetland Rp",
                                     rnat_ept="Terrestrial cover Rp",
                                     dam_ept="Dam",slope_ept="Slope*depth", strpwr_ept="Stream power",
                                     hag_ept="Agr index Rp",nhag_ept="Non-agr index Rp",
                                     precip_ept="Precipitation",tmax_ept="Max temp",
                                     phdi_ept="Drought index",
                                     dexcess_ept="Evaporation indicator",lflow_ept="Summer flow",
                                     bflow_ept="Bankfull flow",xcmgw_ept="Riparian site index",
                                     agr_tot="Agr Ws",urb_tot="Developed Ws",
                                     rfor_tot="Forest Rp",
                                     rwet_tot="Wetland Rp",rnat_tot="Terrestrial cover Rp",
                                     dam_tot="Dam",slope_tot="Slope*depth",
                                     strpwr_tot="Stream power",
                                     hag_tot="Agr index Rp",nhag_tot="Non-agr index Rp",
                                     precip_tot="Precipitation",tmax_tot="Max temp",
                                     phdi_tot="Drought index",
                                     dexcess_tot="Evaporation indicator",lflow_tot="Summer flow",
                                     bflow_tot="Bankfull flow",
                                     tp_tot="TP",tn_tot="TN",sulf_tot="Sulfate", turb_tot="Turbidity",
                                     rbs_tot="Bed stability",xcmgw_tot="Riparian site index",
                                     xfcnat_tot="Instream cover",
                                     agr_tntot="Agr Ws", urb_tntot="Developed Ws",dam_tntot="Dam",precip_tntot="Precipitation",
                                     phdi_tntot="Drought index",slope_tntot="Slope*depth",strpwr_tntot="Stream power",hag_tntot="Agr index Rp",nhag_tntot="Non-agr index Rp",
                                     rfor_tntot="Forest Rp",rnat_tntot="Terrestrial cover Rp", rwet_tntot="Wetland Rp",lflow_tntot="Summer flow",bflow_tntot="Bankfull flow",
                                     dexcess_tntot="Evaporation indicator",xcmgw_tntot="Riparian site index",
                                     urb_rbstot="Developed Ws",agr_rbstot="Agr Ws",dam_rbstot="Dam",precip_rbstot="Precipitation",
                                     phdi_rbstot="Drought index",
                                     slope_rbstot="Slope*depth",strpwr_rbstot="Stream power",hag_rbstot="Agr index Rp",nhag_rbstot="Non-agr index Rp",
                                     rfor_rbstot="Forest Rp",rnat_rbstot="Terrestrial cover Rp",rwet_rbstot="Wetland Rp",
                                     lflow_rbstot="Summer flow",
                                     bflow_rbstot="Bankfull flow", dexcess_rbstot="Evaporation indicator",
                                     xcmgw_rbstot="Riparian site index",
                                     agr_xcmgwtot="Agr Ws", urb_xcmgwtot="Developed Ws",dam_xcmgwtot="Dam",precip_xcmgwtot="Precipitation",
                                     phdi_xcmgwtot="Drought index",slope_xcmgwtot="Slope*depth",strpwr_xcmgwtot="Stream power",hag_xcmgwtot="Agr index Rp",nhag_xcmgwtot="Non-agr index Rp",
                                     rfor_xcmgwtot="Forest Rp", rnat_xcmgwtot="Terrestrial cover Rp",rwet_xcmgwtot="Wetland Rp",lflow_xcmgwtot="Summer flow",
                                     dexcess_xcmgwtot="Evaporation indicator",
                                     agr_sulftot="Agr Ws", urb_sulftot="Developed Ws",dam_sulftot="Dam",precip_sulftot="Precipitation",
                                     phdi_sulftot="Drought index",slope_sulftot="Slope*depth",strpwr_sulftot="Stream power",hag_sulftot="Agr index Rp",nhag_sulftot="Non-agr index Rp",
                                     rfor_sulftot="Forest Rp",rnat_sulftot="Terrestrial cover Rp", rwet_sulftot="Wetland Rp",lflow_sulftot="Summer flow",bflow_sulftot="Bankfull flow",
                                     dexcess_sulftot="Evaporation indicator",xcmgw_sulftot="Riparian site index",
                                     agr_evtot="Agr Ws", urb_evtot="Developed Ws",dam_evtot="Dam",precip_evtot="Precipitation",
                                     phdi_evtot="Drought index",slope_evtot="Slope*depth",strpwr_evtot="Stream power",hag_evtot="Agr index Rp",nhag_evtot="Non-agr index Rp",
                                     rfor_evtot="Forest Rp",rnat_evtot="Terrestrial cover Rp", rwet_evtot="Wetland Rp",lflow_evtot="Summer flow",bflow_evtot="Bankfull flow",
                                     agr_lftot="Agr Ws", urb_lftot="Developed Ws",dam_lftot="Dam",precip_lftot="Precipitation",
                                     phdi_lftot="Drought index",slope_lftot="Slope*depth",strpwr_lftot="Stream power",hag_lftot="Agr index Rp",nhag_lftot="Non-agr index Rp",
                                     rfor_lftot="Forest Rp",rnat_lftot="Terrestrial cover Rp", rwet_lftot="Wetland Rp",
                                     agr_bftot="Agr Ws", urb_bftot="Developed Ws",dam_bftot="Dam",precip_bftot="Precipitation",
                                     phdi_bftot="Drought index",slope_bftot="Slope*depth",strpwr_bftot="Stream power",hag_bftot="Agr index Rp",nhag_bftot="Non-agr index Rp",
                                     rfor_bftot="Forest Rp",rnat_bftot="Terrestrial cover Rp", rwet_bftot="Wetland Rp",
                                     agr_sptot="Agr Ws", urb_sptot="Developed Ws",dam_sptot="Dam",precip_sptot="Precipitation",
                                     phdi_sptot="Drought index",slope_sptot="Slope*depth",strpwr_sptot="Stream power",hag_sptot="Agr index Rp",nhag_sptot="Non-agr index Rp",
                                     rfor_sptot="Forest Rp",rnat_sptot="Terrestrial cover Rp", rwet_sptot="Wetland Rp"))%>%
    rename(Effects = lhs)
}


