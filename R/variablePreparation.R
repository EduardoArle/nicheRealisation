#' variablePreparation
#' 
#' Prepares the standardised versions of the variables that contribute the most
#' in the maxent model for the niche breadth calculation.
#'
#' @importFrom raster stack
#' @param new_path character, the path to the folder containing the 
#' standardised variables.
#' @param var_cont table informing the contribution of each variable in a
#'maxent model, the output of **variableContribution**.
#' @param th numeric, threshold of variable contribution to the model. Default
#' is 10%.
#' @return This function returns a raster stack containing the variables that
#' contributed to the model more than the selected threshold (in percentage).
#' @export
variablePreparation <- function(new_path,var_cont,th = 10){
  
  oldwd <- getwd()
  setwd(new_path)
  variables <- stack(list.files(pattern = ".tif$"))
  var_cont2 <- var_cont[which(var_cont$contribution >= th),]
  variables2 <- variables[[which(names(variables) %in% var_cont2$var)]]
  setwd(oldwd)
  
  return(variables2)
}