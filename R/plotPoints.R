#' plotPoints
#' 
#' Plot occurrence points with background for visualisation.
#'
#' @importFrom graphics points
#' @importFrom raster extent plot
#' @importFrom rgeos gIntersection gBuffer
#' @importFrom rworldmap getMap
#' @importFrom sp CRS over proj4string
#' @importFrom bRacatus giveOcc occSpatialPoints
#' @param occ table containing columns with the species name, longitude, and
#' latitude
#' @param species character, name of the column containing species binomial
#' name. Default is "species".
#' @param lon character, name of the longitude column, default is 
#' "decimalLongitude".
#' @param lat character, name of the latitude column, default is 
#' "decimalLatitude".
#' @param colours character, options are "bicha" or "standard". Default is
#' "standard"
#' @return This function plots occurrences on a background.
#' @export
plotPoints <- function(occ,location="location",
                       lon="decimalLongitude",lat="decimalLatitude",
                       colours = "standard") {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  world <- getMap(resolution = "low")
  world <- suppressWarnings(gBuffer(world, byid = TRUE, width = 0))
  occ2 <- giveOcc(occ,location,lon,lat)
  occ_sp <- occSpatialPoints(occ2)
  countries <- unique(over(occ_sp,world)$NAME)
  countries <- world[world$NAME %in% countries,]
  CP <- as(extent(countries), "SpatialPolygons")
  sp::proj4string(CP) <- sp::CRS(proj4string(world))
  map <- suppressWarnings(gIntersection(world,
                                        CP,
                                        byid = TRUE, 
                                        checkValidity = 2))
  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  if(colours != "bicha"){
    plot(map, col = "khaki", bg = "azure2",
         main = unique(occ_sp$species), font.main = 3)
    points(occ_sp, pch = 21, cex = 1, bg = "red")
  }else{
    plot(map, col = "magenta", bg = "cyan",
         main = unique(occ_sp$species), font.main = 3)
    points(occ_sp, pch = 21, cex = 1, bg = "yellow")
  }
}