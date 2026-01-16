#' Build the Marine Based Water Types Within the Northern Three Region
#'
#' @param basins An sf object produced by the [build_basins()] function
#' @param named_islands An sf object produced by the [build_named_islands()] function
#'
#' @returns An sf object
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' waterbodies <- build_waterbodies(basins, named_islands)
#' }
#' 
build_waterbodies <- function(basins, named_islands) {  
    
  #read the water bodies data set from the aims package
  tmp <- new.env() 
  utils::data("wbodies", package = "gisaimsr", envir = tmp) 

  water_bodies <- tmp$wbodies |> 
    name_cleaning() |> 
    sf::st_transform("EPSG:7855")

  #do a preliminary hole fill and cut to fill unnamed islands
  water_bodies <- water_bodies |> 
    nngeo::st_remove_holes() |> 
    sf::st_difference(sf::st_union(named_islands)) |> 
    dplyr::rename(geom = geometry)

  #create a buffer around the water bodies then take only the buffer, union everything as no info is needed currently
  wb_buffer_dif <- water_bodies |> 
    dplyr::mutate(geom = sf::st_buffer(geom, dist = 4000)) |> 
    sf::st_difference(sf::st_union(water_bodies)) |> 
    sf::st_union() |> 
    sf::st_as_sf()

  #create a buffer around the land basins then take only the buffer, union everything as no info is needed currently
  basin_buffer_dif <- basins |> 
    dplyr::mutate(geom = sf::st_buffer(geom, dist = 4000)) |> 
    sf::st_difference(sf::st_union(basins)) |> 
    sf::st_union() |> 
    sf::st_as_sf()

  #get the area that is shared between the two  zones (this creates a fill layer between the og basin and wb datasets), add info
  area_shared <- sf::st_intersection(basin_buffer_dif, wb_buffer_dif) |> 
    dplyr::mutate(SubZone = "Enclosed Coastal") |> dplyr::rename(geom = x)

  #join the shared area onto the water bodies data. This now fills all gaps between water and land, but has not removed overlaps between water and land
  water_bodies_expanded <- water_bodies |> 
    dplyr::select(MarineWate) |> 
    dplyr::rename(SubZone = MarineWate) |> 
    rbind(area_shared) 

  #make everything valid
  water_bodies_expanded <- sf::st_make_valid(water_bodies_expanded)
  basins <- sf::st_make_valid(basins)
  named_islands <- sf::st_make_valid(named_islands)

  #union the two "subtract" layers into one geometry
  remove_geom <- sf::st_union(sf::st_union(basins), sf::st_union(named_islands))

  #pre-filter water bodies that actually intersect the removal geometry
  idx <- sf::st_intersects(water_bodies_expanded, remove_geom) 
  intersecting <- water_bodies_expanded[lengths(idx) > 0, ] 
  non_intersecting <- water_bodies_expanded[lengths(idx) == 0, ]

  #convert to terra for faster processing
  inter_v <- terra::vect(intersecting)
  rem_v <- terra::vect(remove_geom)
  inter_final_v <- terra::erase(inter_v, rem_v)

  #then convert back
  inter_final <- sf::st_as_sf(inter_final_v) |> 
    name_cleaning()

  #bind the rows that didn't intersect at all with the intersected and processed rows
  water_bodies_final <- rbind(non_intersecting, inter_final)
    
  #streamline subzone names and union by each subzone
  water_bodies_final <- water_bodies_final |> 
    dplyr::mutate(SubZone = dplyr::case_when(
      stringr::str_detect(SubZone, "Enclosed") ~ "Enclosed Coastal",
      stringr::str_detect(SubZone, "Open") ~ "Open Coastal",
      TRUE ~ SubZone
    )) |> 
    dplyr::group_by(SubZone) |> 
    dplyr::summarise(geom = sf::st_union(geom))

  return(water_bodies_final)

}