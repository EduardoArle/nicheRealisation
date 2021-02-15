#' variableStandardisation
#' 
#' Standardise raster layers by subtracting the mean and dividing by the 
#' standard deviation (Cardillo et al. 2018 Journal of Biogeography).
#'
#' @importFrom raster extract stack writeRaster nlayers
#' @param original_path character, the path to the folder containing the 
#' variables.
#' @param new_path character, the path to the folder where the standardised
#' variables shall be saved.
#' @return This function standardises the variables and saves the new version.
#' @export
variableStandardisation <- function(original_path,new_path){
  #get values of all variables in the occurrence points locations
  oldwd <- getwd()
  setwd(original_path)
  variables <- stack(list.files(pattern = ".bil$"))
  
  for(i in 1:nlayers(variables))
  {
    mean_var <- mean(variables[[i]][],na.rm=T)
    sd_var <- sd(variables[[i]][],na.rm=T)
    var2 <- variables[[i]] - mean_var
    var3 <- var2 / sd_var
    
    setwd(new_path)
    writeRaster(var3, filename = paste0(names(var3),".tif"), 
                format="GTiff")
  }
  setwd(oldwd)
}