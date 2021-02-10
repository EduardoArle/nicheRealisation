#' getWorldClim
#' 
#' Downloads bioclimatic data from WorldClim
#'
#' @importFrom raster getData
#' @importFrom rworldmap getMap
#' @param res numeric, resolution of the data. Possible values are 10, 5, 2.5,
#' or 0.5 arc minutes. If res = 0.5, country must be specified.
#' @param country character, country name. Sets the tile containing the central
#' coordinates of the defined country. If NULL, gets variable for whole world.
#' Default is NULL.
#' @param path character, the path to the folder where the variables shall
#' be saved.
#' @return This function downloads all 19 climatic variables and saves them in
#' the folder defined in "path".
#' @examples
#' world_clim <- getWorldClim(res = 0.5,country = "Czech Republic",
#' path = getwd())
#' @export
getWorldClim <- function(res, country = NULL, path) {
  if(is.null(country)){
    worldclim <- getData("worldclim", var = "bio", res = res,
                         path = path)
  }else{
    world <- getMap(resolution = "coarse")
    region <- world[which(world$SOVEREIGNT == country),]
    
    worldclim <- getData("worldclim", var = "bio", res = res,
                         lon = region$LON, lat = region$LAT,
                         path = path)
  }
  return(worldclim)
}