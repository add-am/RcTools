#' Build the Landbased Water Types Within the Northern Three Region
#'
#' @param n3_marine An sf object produced within the [build_waterbody_boundaries()] function
#' @param n3_land An sf object produced within the [build_n3_region()] function
#'
#' @returns An sf object
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' n3_land <- build_water_types(n3_land, n3_marine)
#' }
#' 
build_water_types <- function(n3_land, n3_marine) {

  #load the data in from file
  #epp_water_types <- sf::st_read("epp_water_types.gpkg") |> 
  #  name_cleaning() |> 
  #  sf::st_transform("EPSG:7855")

  #get only the fresh and estuarine watertypes from the EPP data
  #water_types <- epp_water_types |> 
  #  dplyr::filter(!stringr::str_detect(WaterType, "coast|shelf"))|> 
  #  dplyr::mutate(Environment = dplyr::case_when(stringr::str_detect(WaterType, "estua") ~  "Estuarine", TRUE ~ "Freshwater")) |> 
  #  dplyr::select(Environment, geom)

  #create a unionised outline of the n3 land area with a slight buffer
  #n3_outline <- n3_land |> 
  #  sf::st_union() |> 
  #  sf::st_buffer(1) |> 
  #  sf::st_make_valid()

  #crop the water types to an area only a bit larger than the n3 region and reduce file size for saving
  #n3_water_types <- sf::st_crop(water_types, n3_land) |> 
  #  dplyr::group_by(Environment) |> 
  #  dplyr::summarise(geom = st_union(geom)) |> 
  #  sf::st_simplify() |> 
  #  nngeo::st_remove_holes(max_area = 300) |> 
  #  sf::st_make_valid() 
  
  #once cropped and simplified, intersect based on the buffered outline 
  #n3_water_types <- n3_water_types |> 
  #  sf::st_intersection(n3_outline)

  #load in the water type boundaries (above is how this file was originally made)
  load(system.file("extdata/n3_water_types.RData", package = "RcTools"))
  
  #rename the geometry column
  n3_water_types <- dplyr::rename(n3_water_types, geom = geometry)
   
  #get the difference between the water types area and the area for the n3 land (catches areas that n3 land is bigger)
  diff <- n3_land |> 
    sf::st_difference(sf::st_union(n3_water_types))

  #create a point geom for each of the polygons 
  diff$geom2 <- sf::st_centroid(sf::st_geometry(diff))

  #for each of the centroids figure out which of the water types is closest
  diff$NearestWaterType <- sf::st_nearest_feature(diff, n3_water_types)

  #get the index for each water type and the name of each water type
  replace_with <- n3_water_types$Environment
  replace_from <- 1:length(n3_water_types$Environment)

  #match each case and replace with the sub basin
  diff$Environment <- replace_with[match(diff$NearestWaterType, replace_from)]

  #join the difference back on to the main water type set and union everything up
  n3_water_types <- diff |> dplyr::select(Environment, geom) |> 
    rbind(n3_water_types) |> 
    dplyr::group_by(Environment) |> 
    dplyr::summarise(geom = sf::st_union(geom))

  #cut the area back by the n3_marine boundary (catches areas where water type was bigger)
  n3_water_types <- n3_water_types |> 
    sf::st_difference(sf::st_union(n3_marine)) 

  #prep the freshwater environment for a hefty intersection step
  n3_fresh <- n3_water_types |> 
    dplyr::filter(Environment == "Freshwater") |> 
    sf::st_make_valid() |> 
    sf::st_collection_extract("POLYGON") |> 
    nngeo::st_remove_holes() |> 
    sf::st_simplify() |> 
    sf::st_cast("POLYGON") |> 
    sf::st_set_precision(1e6) |> 
    sf::st_make_valid()

  #prep the estuarine environment for a hefty intersection step
  n3_estuarine <- n3_water_types |> 
    dplyr::filter(Environment == "Estuarine") |> 
    sf::st_make_valid() |> 
    sf::st_collection_extract("POLYGON") |> 
    sf::st_simplify() |> 
    sf::st_cast("POLYGON") |> 
    sf::st_set_precision(1e6) |> 
    sf::st_make_valid()

  #prep the main land object for a hefty intersection step
  n3_land <- sf::st_simplify(n3_land) |> 
    sf::st_set_precision(1e6) |> 
    sf::st_make_valid() |> 
    sf::st_collection_extract("POLYGON") 

  #convert to a terra object for faster processing
  n3_fresh <- terra::vect(n3_fresh) 
  n3_estuarine <- terra::vect(n3_estuarine) 
  n3_land <- terra::vect(n3_land)

  #perform the intersection
  fw_combined <- terra::intersect(n3_fresh, n3_land)
  est_combined <- terra::intersect(n3_estuarine, n3_land)

  #convert back to an sf object
  fw_combined <- sf::st_as_sf(fw_combined)
  est_combined <- sf::st_as_sf(est_combined)

  #combine the two objects back into the main land object
  n3_land <- rbind(fw_combined, est_combined) |> 
    dplyr::rename(geom = geometry)

  return(n3_land)
}