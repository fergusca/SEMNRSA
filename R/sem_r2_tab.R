#' Process SEM R2 output for OE,MMI, EPT relabel variable names to make into a table
#'
#' @param sem_output SEM estimated effects output saved as .csv file
#'
#' @return Processed dataframe that selected direct, indirect, and total effects
#' @export
#'
#' @examples
#'west_oe_m1_full<- r2_tab("model1") # name R2 dataframe processed from SEM output
r2_tab = function(R2_output){
  R2_out_proc <- R2_output %>%
    select(lhs,est)%>%
    rename(Response = lhs, R2 = est)%>%
    mutate(across(where(is.numeric), round,2))%>%
    mutate(Response = recode_factor(Response,
                                     Lpt01_XCMGW="Riparian cover",
                                     LQLow_kmcl="Summer flow",LQbkf_kmcl="Bankfull flow",
                                     evap_index_sc= "Evaporation indicator",
                                     Lpt01_XFC_NAT="Instream cover",LRBS_use="Bed stability",
                                     L_NTL="TN", L_SULF="Sulfate",
                                     OE_SCORE="OE", MMI_BENT_sc="MMI", EPT_RICH_sc="EPT"))
}


