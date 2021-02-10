#' getOcc
#' 
#' Uses the **bRacatus** package function.
#'
#' @importFrom bRacatus getOcc
#' @param species character, species binomial name.
#' @return This function downloads all records for a species from GBIF that
#' have coordinates info. If necessary it loops several times to overcome the
#' limit of 200,000 occurrences imposed by occ_search function. It returns a
#' data table.
#' @examples
#' sps_occurrence <- getOcc("Hemitriccus mirandae")
#' @export
getOcc <- function(species) {
  occurrence <- bRacatus::getOcc(species)
  return(occurrence)
}