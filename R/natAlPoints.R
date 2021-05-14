#' natAlPoints
#' 
#' Identifies native and alien points based on overlap with regions in 
#' checklists.
#'
#' @importFrom bRacatus occSpatialPoints
#' @param occ able containing columns with the species name, longitude, and
#' latitude.
#' @param nat_al list containing reference region shapefiles for the native and
#' alien distribution, having all alien regions overlapping native regions 
#' eliminated. Output of function **noNatAlOverlap**
#' @return list containing points overlapping the native regions, and those 
#' overlapping the alien regions.
#' @export
natAlPoints <- function(occ,nat_al){
  occ2 <- occSpatialPoints(occ)
  
  nat_pts <- over(occ2,nat_al$native)  #identify points in the native regions
  nat_pts2 <- cbind(occ,nat_pts)
  nat_pts3 <- nat_pts2[which(!is.na(nat_pts[,1])),]
  
  alien_pts <- over(occ2,nat_al$alien)  #identify points in the native regions
  alien_pts2 <- cbind(occ,alien_pts)
  alien_pts3 <- alien_pts2[which(!is.na(alien_pts[,1])),]
  
  nat_alien_pts <- list(nat_pts3,alien_pts3)
  names(nat_alien_pts) <- c("native","alien")
  return(nat_alien_pts)
}