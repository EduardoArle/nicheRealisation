#' extractValues
#'
#' Extracts bioclimatic data from WorldClim layers for data points.
#'
#' @importFrom bRacatus giveOcc occSpatialPoints
#' @importFrom raster extract stack
#' @param path character, the path to the folder containing the variables.
#' @param occ table containing columns with the species name, longitude, and
#' latitude.
#' @param location character, name of the column containing location IDs,
#' default is "species".
#' @param lon character, name of the longitude column, default is "default is 
#' "decimalLongitude".
#' @param lat character, name of the latitude column, default is default is 
#' "decimalLatitude".
#' @param original_format character, indicates the format of the layers.
#' @return This function returns the values for variables in each point.
#' @export
extractValues <- function(path,occ,location="species",
                          lon="decimalLongitude",lat="decimalLatitude",
                          original_format = ".asc") {
  
  t <- giveOcc(occ,location,lon,lat)
  t2 <- occSpatialPoints(t)
  
  oldwd <- getwd()
  setwd(path)
  variables <- stack(list.files(pattern = original_format))
  setwd(oldwd)
  
  values <- extract(variables,t2)
  table <- cbind(t,values)
  
  return(table)
}
