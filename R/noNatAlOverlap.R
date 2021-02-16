#' noNatAlOverlap
#' 
#' Eliminate from the shapefiles regions listed as alien that overlap
#' regions listed as native.
#'
#' @importFrom sp over spsample
#' @param ref_reg list containing three reference region shapefiles showing the
#' regions where the species is present, native, and alien. Output of the 
#' **bRacatus** function **giftRegion**.
#' @return list containing reference region shapefiles for the native and alien
#' distribution, having all alien regions overlapping native regions 
#' eliminated.
#' @export
noNatAlOverlap <- function(ref_reg){
  #get values of all variables in the occurrence points locations
  native <- ref_reg$Native
  alien <- ref_reg$Alien
  
  points <- spsample(native,n=1000,type="random",iter=100) #seed random points in native regions
  points2 <- over(points,alien) #identify alien regions overlapping native regions
  
  eliminate <- which(alien$entity_ID %in% unique(points2$entity_ID))
  
  if(length(eliminate) > 0){
    alien2 <- alien[-eliminate,]
  }else{
    alien2 <- alien
  }
  
  nat_alien <- list(native,alien2)
  names(nat_alien) <- c("native","alien")
  return(nat_alien)
}