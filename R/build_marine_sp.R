#' Apply Special Transformations to the n3_marine Dataset
#'
#' @param n3_marine An sf object produced within the [build_waterbody_boundaries()] function
#' @param n3_land An sf object produced within the [build_n3_region()] function
#'
#' @returns An sf object
#'
#' @examples
#' n3_marine <- build_marine_sp(n3_marine, n3_land)
#' 
build_marine_sp <- function(n3_marine, n3_land) {

  #load the data in from file
  #epp_water_manage <- sf::st_read("epp_water_manage.gpkg") |> 
  #  name_cleaning() |> 
  #  sf::st_transform("EPSG:7855")

  #select the two special areas (magnetic island water and port boundary)
  #special_areas <- epp_water_manage |> 
  #  dplyr::filter(
  #    MiId %in% c("SD2244", "SD2243", "MD2241", "MD2242") & EnvValueZone == "Halifax & Cleveland Bay" |
  #      MiId != "SD2244" & MiId != "SD2243" & EnvValueZone == "Halifax & Cleveland Bay"
  #  ) |> 
  #  dplyr::mutate(MiId = dplyr::case_when(!MiId %in% c("SD2244", "SD2243", "MD2241", "MD2242") ~ "General", TRUE ~ MiId)) |> 
  #  dplyr::group_by(MiId) |> 
  #  dplyr::mutate(geom = st_union(geom))

  #load in the data (above is how this file was originally made)
  load(system.file("extdata/special_areas.RData", package = "RcTools"))

  #buffer the magnetic island waters to fill gaps between the water and the land
  mi_buffer <- special_areas |> 
    dplyr::filter(MiId %in% c("SD2244", "SD2243")) |> 
      sf::st_buffer(dist = 200) |> 
      dplyr::select()
  
  #cut the buffer back by the land component
  mi_buffer <- mi_buffer |> 
    sf::st_difference(sf::st_union(n3_land)) 
  
  #then fetch the surrounding waters
  surrounding <- special_areas |> 
    dplyr::filter(MiId == "General") |> 
    dplyr::select()

  #then cut by the surrounding waters component to make the magnetic waters perfectly fit 
  mi_buffer <- mi_buffer |> 
    sf::st_difference(surrounding)

  #give the magnetic island its required columns
  mi_buffer <- mi_buffer |>  
    dplyr::mutate(
      Region = "Dry Tropics", Environment = "Marine", Zone = "Cleveland Bay", SubZone = "Magnetic Island"
    )

  #cut a hole in the marine layer with the Maggie island waters
  n3_marine <- sf::st_difference(n3_marine, sf::st_union(mi_buffer))

  #put the waters in the cut out marine layer
  n3_marine <- rbind(n3_marine, mi_buffer)
    
  #get the port zone and buffer to fill the gap between the port zone and land
  pz_buffer <- special_areas |> 
    dplyr::filter(MiId == "MD2241") |> 
    sf::st_buffer(dist = 200) |> 
    dplyr::select()    

  #cut the buffer back by the land component, 
  pz_buffer <- pz_buffer |> 
    sf::st_difference(sf::st_union(n3_land)) 

  #then fetch the surrounding waters
  surrounding <- special_areas |> 
    dplyr::filter(MiId == "MD2242") |> 
    dplyr::select()

  #then cut by the surrounding waters component to make the magnetic waters perfectly fit 
  pz_buffer <- pz_buffer |> 
    sf::st_difference(surrounding)

  #give the port zone its enclosed and open delegation and add the geographic area
  pz_buffer <- pz_buffer |> 
    sf::st_intersection(n3_marine) |> 
    dplyr::mutate(
      WatercourseOrGeographicArea = dplyr::case_when(
        SubZone == "Open Coastal" ~ "OC.Inside Port Zone",
        TRUE ~ "EC.Inside Port Zone"))

  #cut a hole in the marine layer with the port zone and add the geographic area
  n3_marine <- n3_marine |> 
    sf::st_difference(sf::st_union(pz_buffer)) |> 
    dplyr::mutate(
      WatercourseOrGeographicArea = dplyr::case_when(
        SubZone == "Enclosed Coastal" & Region == "Dry Tropics" & Zone == "Cleveland Bay" ~ "EC.Outside Port Zone",
        SubZone == "Open Coastal" & Region == "Dry Tropics" & Zone == "Cleveland Bay" ~ "OC.Outside Port Zone",
        TRUE ~ SubZone))

  #put the port zone in the cut out marine layer
  n3_marine <- rbind(n3_marine, pz_buffer)  

  return(n3_marine)

}