#' Build the Named Islands Within the Northern Three Region
#'
#' @param basins An sf object produced by the [build_basins()] function
#'
#' @returns An sf object
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' named_islands <- build_named_islands(basins)
#' }
#' 
build_named_islands <- function(basins) {  

  #bring in all islands (that each have their own polygon), 
  tmp <- new.env() 
  utils::data("gbr_feat", package = "gisaimsr", envir = tmp) 

  named_islands <- tmp$gbr_feat |> 
    name_cleaning() |> 
    sf::st_transform("EPSG:7855")

  #and filter for named islands
  named_islands <- named_islands |> 
    dplyr::filter(FeatName == "Island" & GbrName != "U/N Island")|> 
    dplyr::mutate(geom2 = sf::st_centroid(geom)) |> 
    dplyr::select(Objectid, geom, geom2)
    
  #referencing the point geom, categorize each of the islands into a specific region
  named_islands <- named_islands |> 
      dplyr::mutate(
        Region = dplyr::case_when(
          sf::st_coordinates(geom2)[,2] > 7949996 ~ "Wet Tropics",
          sf::st_coordinates(geom2)[,2] < 7949996 & sf::st_coordinates(geom2)[,2] > 7865902 & sf::st_coordinates(geom2)[,1] > 443371.1 ~ "Dry Tropics",
          sf::st_coordinates(geom2)[,2] < 7819426 & sf::st_coordinates(geom2)[,2] > 7552699 ~ "Mackay Whitsunday Isaac")) |> 
    tidyr::drop_na(Region)
  
  #remove a few instances that overlap with the mainland
  named_islands <- dplyr::filter(named_islands, !Objectid %in% c(606, 539, 4960))

  #Create a integer of the nearest basin to each island
  nearest_basin <- sf::st_nearest_feature(named_islands, basins)

  #create a matching system for the integers
  replace_with <- unique(basins$BasinName)
  replace_from <- c(1:length(unique(basins$BasinName)))

  #include which basins each island belongs to
  named_islands$basinsName <- c(replace_with, nearest_basin)[match(nearest_basin, c(replace_from, nearest_basin))]

  #determined the closest basin, and override the palm island group to give them to the dry tropics, also add a unique names for dt islands
  named_islands <- named_islands |> 
    dplyr::mutate(
      BasinName = c(replace_with, nearest_basin)[match(nearest_basin, c(replace_from, nearest_basin))],
      BasinName = dplyr::case_when(
        Region == "Dry Tropics" & BasinName == "Herbert" ~ "Palm Islands",
        Region == "Dry Tropics" & BasinName == "Black" ~ "Palm Islands",
        BasinName == "Ross" ~ "Magnetic Island",
        TRUE ~ BasinName)
      ) |> 
    dplyr::select(Region, BasinName) 
  
  #keep only islands within the bounding box of the land basins
  basin_extent <- sf::st_bbox(basins)
  basin_extent[3] <- 7897976
  named_islands <- sf::st_crop(named_islands, basin_extent)

  return(named_islands)

}
