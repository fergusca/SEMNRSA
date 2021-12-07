#' Merge multiple NARS datasets by SITE_ID and VISIT_NO
#'
#' @param mergeme A folder where processed datasets are stored. Typically would select
#' the variables of interest and save the modified dataset in a separate folder with other processed
#' datasets to merge together
#'
#' @return A data frame with the merged datasets joined based on SITE_ID and VISIT_NO
#' @export
#'
#' @examples
#'nrsa<- multimerge("processed_data") # name of folder with processed data to merge
multimerge = function(mergeme){
  filenames=list.files(path=mergeme, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y, by=c("SITE_ID","VISIT_NO"))}, datalist)
}
