#' variableContribution
#' 
#' Tests of the environmental variable importance for a given species based on
#' Maxent algorithm.
#'
#' @importFrom dismo kfold maxent
#' @importFrom raster stack
#' @param occ table containing columns with the species name, longitude, and
#' latitude.
#' @param path character, the path to the folder containing the variables.
#' @return This function return a table informing the contribution of each
#' variable in a maxent model.
#' @export
variableContribution <- function(occ,path){
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
                            
  #obtain and organise variable importance
  var_imp <- plot(me)
  var_imp2 <- sort(var_imp, decreasing = TRUE)
  var_imp3 <- data.frame(var=names(var_imp2),contribution=var_imp2)
  
  return(var_imp3)                          
}