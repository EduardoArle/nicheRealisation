#' jackknifeMaxent
#' 
#' Tests of the environmental variable importance for a given species based on
#' Maxent algorithm.
#'
#' @importFrom dismo kfold
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
jackknifeMaxent <- function(occ,location="location",
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

  head(occ)
  
  #select lon and lat columns and standardise names for maxent
  occ2 <- occ[,c("decimalLongitude","decimalLatitude")]
  names(occ2) <- c("lon","lat")

  #separate data into train and test
  fold <- kfold(occ2, k=5)
  occtrain <- occ2[fold != 1, ]
  occtest <- occ2[fold == 1, ]
  
  #load variables
  oldwd <- getwd()
  setwd(path)
  predictors <- stack(list.files(pattern = ".bil$"))
  setwd(oldwd)
  
  #run maxent model
  me <- maxent(predictors, occtrain) 
  
  #obtain variable importance
  var_imp <- plot(me)
  class(var_imp)
  
  t <- sort(var_imp, decreasing = TRUE)
  names(t)
  

  maxent_pr <- predict(me, predictors) #predict suitability with maxent

  th <- threshold_selection(bg,occtest,maxent_pr[[i]])

  maxent_pr_bin[[i]] <- maxent_pr[[i]]

  maxent_pr_bin[[i]][] <- ifelse(maxent_pr_bin[[i]][] >= th, 1, 0)

  rast_pts <- as.data.frame(rasterToPoints(maxent_pr_bin[[i]]))

  rast_pts2 <- rast_pts[which(rast_pts$layer == 1),]

  coordinates(rast_pts2) <- ~x+y
  proj4string(rast_pts2) <- CRS(as.character(crs(prob_occ)))

  ext_values <- as.data.frame(extract(predictors,rast_pts2))

  n_breadth2[[i]] <- abs(max(ext_values$bio1)-min(ext_values$bio1)) *
    abs(max(ext_values$bio12)-min(ext_values$bio12))

}
plot(maxent_pr[[10]])
