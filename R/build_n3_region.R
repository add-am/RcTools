#' Build the Northern Three Region Dataset
#'
#' @returns An sf object
#'
#' @export
#' @examples
#' \dontrun{ #dont run because function takes a while
#' n3_region <- build_n3_region()
#' }
#' 
build_n3_region <- function(){

  message("Building Northern Three Basins...")
  basins <- build_basins()
  Sys.sleep(0.5)

  message("Building Northern Three Islands...")
  named_islands <- build_named_islands(basins)
  Sys.sleep(0.5)

  message("Building Northern Three Marine Boundary...")
  waterbodies <- build_waterbodies(basins, named_islands)
  Sys.sleep(0.5)

  message("Creating Internal Marine Boundaries...")
  n3_marine <- build_waterbody_boundaries(waterbodies)
  Sys.sleep(0.5)

  #combine basins and islands
  n3_land <- basins |> 
    dplyr::select(Region, BasinName) |> 
    rbind(named_islands)

  message("Building Northern Three Sub Basins...")
  n3_land <- build_sub_basins(n3_land, basins, n3_marine)
  Sys.sleep(0.5)

  message("Assigning Freshwater and Estuarine Zonations...")
  n3_land <- build_water_types(n3_land, n3_marine)
  Sys.sleep(0.5)

  message("Adding Special Land Features...")
  n3_land <- build_land_sp(n3_land)
  Sys.sleep(0.5)

  message("Adding Special Marine Features...")
  n3_marine <- build_marine_sp(n3_marine, n3_land)
  Sys.sleep(0.5)

  #clean names
  n3_marine <- n3_marine |> 
    dplyr::rename(BasinOrZone = Zone, SubBasinOrSubZone = SubZone) 
  
  #clean names
  n3_land <- n3_land |> 
    dplyr::rename(BasinOrZone = BasinName, SubBasinOrSubZone = SubBasin) 

  #combine the n3 land and marine datasets together, reorder and union all polygons that belong to the same group
  n3_land_marine <- rbind(n3_marine, n3_land) |> 
    dplyr::group_by(
      Region, 
      Environment, 
      BasinOrZone, 
      SubBasinOrSubZone, 
      WatercourseOrGeographicArea) |> 
    dplyr::summarise(geom = sf::st_union(geom)) |> 
    sf::st_make_valid()

  #clean up a few missnamed slivers
  n3_land_marine <- n3_land_marine |> 
    dplyr::filter(
      !(BasinOrZone == "Ross" & SubBasinOrSubZone == "Black River"),
      !(BasinOrZone == "Black" & SubBasinOrSubZone == "Ross River (Upper)")
    )

  #return the final output
  return(n3_land_marine)

}
