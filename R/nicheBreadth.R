#' nicheBreadth
#' 
#' Calculate the n-dimensional niche breadth based on point records and
#' environmental layers.
#'
#' @importFrom bRacatus occSpatialPoints
#' @importFrom raster extract
#' @param occ table containing columns with the species name, longitude, and
#' latitude.
#' @param vars2 raster stack containing the variables that contributed most to 
#' the model. The output of **variablePreparation**.
#' @return This function returns the niche breadth calculated based on the 
#' variables' values in the occ locations.
#' @export
nicheBreadth <- function(occ,vars2){
  #get values of all variables in the occurrence points locations
  occ2 <- occSpatialPoints(occ)
  
  values <- as.data.frame(extract(vars2,occ2))
  
  #calculate amplitude of each variable
  
  amp <- numeric()
  for(i in 1:ncol(values))
  {
    amp[i] <- abs(max(values[,i],na.rm=T)-min(values[,i],na.rm=T))
  }
  n_breadth <- prod(amp)
  return(n_breadth)
}