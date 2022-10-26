#' variableContribution
#' 
#' Tests of the environmental variable importance for a given species based on
#' Maxent algorithm.
#'
#' @importFrom dismo maxent
#' @importFrom raster stack
#' @param occ table containing columns with the species name, longitude, and
#' latitude.
#' @param path character, the path to the folder containing the variables.
#' @param corel table, output of the function **variableCorrelation**.
#' @param original_format character, indicates the format of the layers.
#' @return This function return a table informing the contribution of each
#' variable in a maxent model.
#' @export
variableContribution <- function(occ, path, corel, original_format = ".asc"){
  #select lon and lat columns and standardise names for maxent
  occ2 <- occ[,c("decimalLongitude","decimalLatitude")]
  names(occ2) <- c("lon","lat")
                            
  #load variables
  oldwd <- getwd()
  setwd(path)
  vars <- stack(list.files(pattern = original_format))
  setwd(oldwd)
  
  #select variables that are not more correlated than 0.7
  predictors <- vars[[which(names(vars) %in% corel$Variables)]]
                            
  #run maxent model
  maxent()
  me <- maxent(predictors, occ2) 
                            
  #obtain and organise variable importance
  var_imp <- plot(me)
  var_imp2 <- sort(var_imp, decreasing = TRUE)
  var_imp3 <- data.frame(var=names(var_imp2),contribution=var_imp2)
  
  return(var_imp3)                          
}