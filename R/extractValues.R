#' extractValues
#'
#' Extracts bioclimatic data from WorldClim layers for data points.
#'
#' @importFrom bRacatus getData
#' @importFrom rworldmap getMap
#' @param variables output of the function getWorldClim
#' @param occ table containing columns with the species name, longitude, and
#' latitude
#' @param specie character, name of the column containing species names,
#' default is "species".
#' @param lon character, name of the longitude column, default is "lon".
#' @param lat character, name of the latitude column, default is "lat".
#' @return This function downloads all 19 climatic variables and returns them
#' in a raster stack.
#' @examples
#' world_clim <- getWorldClim(res = 0.5,country = "Czech Republic",
#' path = getwd())
#' @export
extractValues <- function(variables,occ,species=species,lon=lon,lat=lat) {
  
  t <- giveOcc(occ,"species","lon","lat")
  t2 <- occSpatialPoints(t)
  
  values <- extract(variables,t2)
  
  return(worldclim)
}

plot(world_clim[[1]])

occ <- data.frame(species="morcego",lat=c(49,50,50),lon=c(13.5,14,15))

t <- giveOcc(fk_points,"species","lon","lat")

t2 <- occSpatialPoints(t)

plotOcc(t)

variables <- world_clim
