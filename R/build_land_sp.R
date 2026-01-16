#' Apply Special Transformations to the n3_land Dataset
#'
#' @param n3_land An sf object produced within the [build_n3_region()] function
#'
#' @returns An sf object
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' n3_land <- build_land_sp(n3_land)
#' }
#' 
build_land_sp <- function(n3_land) {

  #epp_water_env_value <- sf::st_read("epp_water_env_value.gpkg") |> 
  #  name_cleaning() |> 
  #  sf::st_transform("EPSG:7855")

  #select paluma
  #paluma <- epp_water_env_value |> 
  #  dplyr::filter(EnvValueZone == "Paluma Reservoir") |> 
  #  dplyr::mutate(
  #    Region = "Dry Tropics", BasinName = "Black", SubBasin = "Paluma Lake", 
  #    WatercourseOrGeographicArea = NA, Environment = "Freshwater") |> 
  #  dplyr::select(Region, Environment, BasinName, SubBasin, WatercourseOrGeographicArea, geom)

  #load in the data (above is how this file was originally made)
  load(system.file("extdata/paluma.RData", package = "RcTools"))

  #cut a hole in the main data
  n3_land <- sf::st_difference(n3_land, sf::st_union(paluma))

  #insert paluma lake
  n3_land <- rbind(n3_land, paluma)

  return(n3_land)
}