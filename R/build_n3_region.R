#' Build the Northern Three Region Dataset
#'
#' @returns An sf object
#'
#' @export
#' @examples
#' #' n3_region <- build_n3_region()
#' 
build_n3_region <- function(){

  #build the basins dataset
  basins <- build_basins()

  #build the islands dataset
  named_islands <- build_named_islands(basins)

  #build the waterbodies dataset
  waterbodies <- build_waterbodies(basins, named_islands)

  #create internal waterbody boundaries
  n3_marine <- build_waterbody_boundaries(waterbodies)

  #combine the basins and islands objects
  n3_land <- basins |> 
    dplyr::select(Region, BasinName) |> 
    rbind(named_islands)

  #create sub basins within the n3_land dataset
  n3_land <- build_sub_basins(n3_land, basins, n3_marine)

  #create water types (fresh, estuarine) within the n3_land dataset
  n3_land <- build_water_types(n3_land, n3_marine)

  #create special locations in the n3_land dataset
  n3_land <- build_land_sp(n3_land)

  #create special locations in the n3_marine dataset
  n3_marine <- build_marine_sp(n3_marine, n3_land)

  #clean names
  n3_marine <- n3_marine |> 
    dplyr::rename(BasinOrZone = Zone, SubBasinOrSubZone = SubZone) 
  
  #clean names
  n3_land <- n3_land |> 
    dplyr::rename(BasinOrZone = BasinName, SubBasinOrSubZone = SubBasin) 

  #combine the n3 land and marine datasets together, reorder and union all polygons that belong to the same group
  n3_land_marine <- rbind(n3_marine, n3_land) |> 
    dplyr::group_by(
      .data$Region, 
      .data$Environment, 
      .data$BasinOrZone, 
      .data$SubBasinOrSubZone, 
      .data$WatercourseOrGeographicArea) |> 
    dplyr::summarise(geom = sf::st_union(geom)) |> 
    sf::st_make_valid()

  #return the final output
  return(n3_land_marine)

}
