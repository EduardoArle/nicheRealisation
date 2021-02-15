#' nicheBreadth
#' 
#' Calculate the n-dimensional niche breadth based on point records and
#' environmental layers.
#'
#' @importFrom bRacatus occSpatialPoints
#' @importFrom raster extract stack
#' @param occ table containing columns with the species name, longitude, and
#' latitude.
#' @param path character, the path to the folder containing the variables.
#' @param var_cont table informing the contribution of each variable in a
#'maxent model, the output of **variableContribution**.
#' @return This function return a table informing which variables are less
#' correlated than the defined threshold.
#' @export
nicheBreadth <- function(occ,path,var_cont){
  #get values of all variables in the occurrence points locations
  vars <- extractValues(path,occ)
  vars2 <- vars[,-which(names(vars) %in% names(occ))]
  vars3 <- vars2[-which(is.na(vars2)),]
  
  corel <- vifcor(vars3,th=0.7)
  
  return(corel@results)
}