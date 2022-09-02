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
                           str_detect(lhs, "_xcmtot") ~ "Total XCMGW",
                           str_detect(lhs,"_tntot") ~ "Total TN",
                           str_detect(lhs,"_rbstot") ~ "Total RBS"))%>%
    mutate(Predictor = recode_factor(Predictor,
                                     a1="Agr Ws",u1="Urban Ws",sd1="Slope*depth",r1="Bed stability",
                                     f1="Forest Rp",l1="Summer flow",x1="Riparian cover",xn1="Instream cover",
                                     na1="Non-agr index Rp", p1="Precipitation",t1="Max temp", ph1="Drought index",
                                     n1="TN",su1="Sulfate", tu1="Turbidity",
                                     agr_oe="Agr Ws",urb_oe="Urban Ws",
                                     rfor_oe="Forest Rp",
                                     rwet_oe="Wetland Rp",
                                     dam_oe="Dam",slope_oe="Slope*depth",
                                     hag_oe="Agr index Rp",nhag_oe="Non-agr index Rp",
                                     precip_oe="Precipitation",tmax_oe="Max temp",
                                     phdi_oe="Drought index",
                                     dexcess_oe="Evaporation indicator",lflow_oe="Summer flow",
                                     bflow_oe="Bankfull flow",xcmgw_oe="Riparian cover",
                                     agr_mmi="Agr Ws",urb_mmi="Urban Ws",
                                     rfor_mmi="Forest Rp",
                                     rwet_mmi="Wetland Rp",
                                     dam_mmi="Dam",slope_mmi="Slope*depth",
                                     hag_mmi="Agr index Rp",nhag_mmi="Non-agr index Rp",
                                     precip_mmi="Precipitation",tmax_mmi="Max temp",
                                     phdi_mmi="Drought index",
                                     dexcess_mmi="Evaporation indicator",lflow_mmi="Summer flow",
                                     bflow_mmi="Bankfull flow",xcmgw_mmi="Riparian cover",
                                     agr_ept="Agr Ws",urb_ept="Urban Ws",
                                     rfor_ept="Forest Rp",
                                     rwet_ept="Wetland Rp",
                                     dam_ept="Dam",slope_ept="Slope*depth",
                                     hag_ept="Agr index Rp",nhag_ept="Non-agr index Rp",
                                     precip_ept="Precipitation",tmax_ept="Max temp",
                                     phdi_ept="Drought index",
                                     dexcess_ept="Evaporation indicator",lflow_ept="Summer flow",
                                     bflow_ept="Bankfull flow",xcmgw_ept="Riparian cover",
                                     agr_tot="Agr Ws",urb_tot="Urban Ws",
                                     rfor_tot="Forest Rp",
                                     rwet_tot="Wetland Rp",
                                     dam_tot="Dam",slope_tot="Slope*depth",
                                     hag_tot="Agr index Rp",nhag_tot="Non-agr index Rp",
                                     precip_tot="Precipitation",tmax_tot="Max temp",
                                     phdi_tot="Drought index",
                                     dexcess_tot="Evaporation indicator",lflow_tot="Summer flow",
                                     bflow_tot="Bankfull flow",
                                     tp_tot="TP",tn_tot="TN",sulf_tot="Sulfate", turb_tot="Turbidity",
                                     rbs_tot="Bed stability",xcmgw_tot="Riparian cover",
                                     xfcnat_tot="Instream cover",
                                     ag_tntot="Agr Ws",urb_tntot="Urb Ws",dam_tntot="Dam",precip_tntot="Precipitation",
                                     phdi_tntot="Drought index",slope_tntot="Slope*depth",hag_tntot="Agr index Rp",
                                     rfor_tntot="Forest Rp",rwet_tntot="Wetland Rp",lflow_tntot="Summer flow",
                                     dexc_tntot="Evaporation indicator",
                                     urb_rbstot="Urban Ws",agr_rbstot="Agr Ws",dam_rbstot="Dam",precip_rbstot="Precipitation",
                                     phdi_rbstot="Drought index",
                                     slope_rbstot="Slope*depth",hag_rbstot="Agr index Rp",rfor_rbstot="Forest Rp",rwet_rbstot="Wetland Rp",
                                     lflow_rbstot="Summer flow",
                                     bflow_rbstot="Bankfull flow", dexc_rbstot="Evaporation indicator",
                                     xcmgw_rbstot="Riparian cover"))%>%
    rename(Effects = lhs)
}


