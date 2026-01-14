#' Build Basins in the Northern Three region
#'
#' @returns An sf object
#'
#' @examples
#' basin_output <- build_basins()
#' 
build_basins <- function() {

  #create basins and sub basins list for northern three regions
  basin_list <- c("'Don', 'Proserpine', 'O''Connell', 'Pioneer', 'Plane', 'Daintree', 'Mossman', 
    'Barron', 'Johnstone', 'Tully', 'Murray', 'Herbert', 'Ross', 'Black', 'Haughton', 'Burdekin'")
  sub_list <- c("'Russell River', 'Mulgrave River'")

  #build the filter requests (takes the format of an SQL argument)
  basin_query <- glue::glue("BASIN_NAME IN ({basin_list})")
  sub_query <- glue::glue("SUB_NAME IN ({sub_list})")
  
  #extract basins and sub basins from arc gis server:
  basins <- arcgislayers::arc_open("https://spatial-gis.information.qld.gov.au/arcgis/rest/services/InlandWaters/DrainageBasins/MapServer/1")
  subs <- arcgislayers::arc_open("https://spatial-gis.information.qld.gov.au/arcgis/rest/services/InlandWaters/DrainageBasins/MapServer/2")

  #filter the layer based on user selection
  basins_extract <- arcgislayers::arc_select(basins, where = basin_query) |> 
    name_cleaning() |> 
    sf::st_transform("EPSG:7855")
  subs_extract <- arcgislayers::arc_select(subs, where = sub_query) |> 
    name_cleaning()|> 
    sf::st_transform("EPSG:7855")

  #clean data and combine
  basin_and_sub <- subs_extract |> 
    dplyr::rename("BasinName" = "SubName", "BasinNumber" = "SubNumber") |> 
    rbind(basins_extract) |> 
    sf::st_transform("EPSG:7855") |> 
    dplyr::mutate(Region = dplyr::case_when(
      BasinName %in% c("Ross", "Black") ~ "Dry Tropics",
      BasinName %in% c("Burdekin", "Haughton") ~ "Burdekin",
      BasinName %in% c("Plane", "Don", "Proserpine", "Pioneer", "O'Connell") ~ "Mackay Whitsunday Isaac",
      TRUE ~ "Wet Tropics"
    ))
  
  return(basin_and_sub)
}