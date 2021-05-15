#' variableCorrelation
#' 
#' Checks the correlation amongst the variables that contributes most to the
#' species modelling with maxent.
#'
#' @importFrom usdm vifcor
#' @param occ table containing columns with the species name, longitude, and
#' latitude.
#' @param path character, the path to the folder containing the variables.
#' @param th threshold of correlation. Default is 0.7.
#' @return This function return a table informing which variables are less
#' correlated than the defined threshold.
#' @export
variableCorrelation <- function(occ,path,th = 0.7){
  #get values of all variables in the occurrence points locations
  vars <- extractValues(path, occ_clean)
  vars2 <- vars[, -which(names(vars) %in% names(occ_clean))]
  if(length(unique(which(is.na(vars2)))) != 0){
    vars3 <- vars2[-is.na(vars2), ]
  }else{
    vars3 <- vars2
  }
  corel <- vifcor(vars3, th = 0.7)
  
  return(corel@results)
}
g