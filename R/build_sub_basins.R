#' Build the Sub Basins Within the Northern Three Region
#'
#' @param n3_land An sf object produced within the [build_n3_region()] function
#' @param basins An sf object produced by the [build_basins()] function
#' @param n3_marine An sf object produced within the [build_waterbody_boundaries()] function
#'
#' @returns An sf object
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' n3_land <- build_sub_basins(n3_land, basins, n3_marine)
#' }
#' 
build_sub_basins <- function(n3_land, basins, n3_marine) {

  #load in epp datasets from file
  #epp_water_env_value <- sf::st_read("epp_water_env_value.gpkg") |> 
  #  name_cleaning() |> 
  #  sf::st_transform("EPSG:7855")
 
  #select data in dry tropics region (land only).
  #dt_sub_basins <- epp_water_env_value |> 
  #  dplyr::filter(ProjectName == "Townsville Region" & EnvValueZone != "Halifax & Cleveland Bay") |> 
  #  dplyr::mutate(
  #    Region = "Dry Tropics",
  #    BasinName = dplyr::case_when(
  #      stringr::str_detect(BasinName, "Black") ~ "Black",
  #      stringr::str_detect(BasinName, "Ross") ~ "Ross"))
  
  #load in the dt sub basins (above is how this file was originally made)
  load(system.file("extdata/dt_sub_basins.RData", package = "RcTools"))
 
  #create a list of sub basins
  sub_basin <- c(
    "Alligator Creek", "Black River", "Bluewater Creek", "Bohle River", "Crystal Creek", "Magnetic Island",
    "Rollingstone Creek", "Ross River (Lower)", "Ross River (Upper)", "Stuart Creek"
  )
 
  #create a list of ENV_VALUE_ZONE that will be combined into each sub basin
  group_list <- c(
    "Whites|Slippery Rocks|Alligator|Killymoon|Crocodile|Cape Cleveland",
    "Alick|Alice|Canal|Log Creek|Black River",
    "Healy|Althaus|Deep Creek|Bluewater|Two Mile|Christmas",
    "Bohle River|Stony|(lower)|Louisa|Town Common",
    "Lorna|Crystal",
    "Bay|Retreat|China|Duck|Ned|Butlers|Gustav|Petersen|Magnetic|Gorge|Endeavour",
    "Leichhardt Creek|Camp Oven|Saltwater|Station|Surveyors|Rollingstone Creek",
    "Training Area|below dam|Ross Creek|Pallarenda",
    "Lagoon|Round Mountain|Six Mile|One Mile|Toonpan|Blacksoil|Sachs|Lake Ross|Anthill Plains",  
    "Stuart Creek|Townsville State Development|Sandfly"
  )

  #for each patttern detected add the correct sub basin
  dt_sb_grouped <- dt_sub_basins |> 
    dplyr::mutate(
      SubBasin = purrr::reduce(
        purrr::map2(group_list, sub_basin, \(x, y) { 
          dplyr::if_else(stringr::str_detect(EnvValueZone, x), y, NA_character_)
        }),
        dplyr::coalesce
      )
    )
  
  #group up and summarise all geometries (keep the TSDA separate)
  dt_sb_grouped <- dt_sb_grouped |> 
    dplyr::mutate(
      EnvValueZone = dplyr::case_when(
        stringr::str_detect(EnvValueZone, "Townsville State Development Area") ~ EnvValueZone, 
        TRUE ~ NA
      )) |> 
    dplyr::group_by(Region, BasinName, SubBasin, EnvValueZone) |> 
    dplyr::summarise(geom = sf::st_union(geom))
  
  #get the difference between the dt_sub_basin area and the area for ross and black in the n3 files
  diff <- basins |> 
    dplyr::filter(BasinName %in% c("Ross", "Black")) |> 
    sf::st_difference(sf::st_union(dt_sb_grouped)) |> 
    sf::st_cast("POLYGON")

  #create a point geom for each of the polygons 
  diff$geom2 <- sf::st_centroid(sf::st_geometry(diff))

  #for each of the centroids figure out which of the dt_sub basins is closest (excluding TSDA first)
  diff$nearest_sub_basin <- sf::st_nearest_feature(diff, dplyr::filter(dt_sb_grouped, is.na(EnvValueZone)))

  #get the index for each sub basin and the name of each sub basin
  replace_with <- unique(dt_sb_grouped$SubBasin)
  replace_from <- c(1:length(unique(dt_sb_grouped$SubBasin)))

  #match each case and replace with the sub basin
  diff$SubBasin <- replace_with[match(diff$nearest_sub_basin, replace_from)]

  #fix the number of columns in the diff table
  diff$EnvValueZone <- NA

  #join the difference back on to the main sub basin set and union everything up
  dt_sb_grouped <- diff |> 
    dplyr::select(Region, BasinName, SubBasin, EnvValueZone, geom) |> 
    rbind(dt_sb_grouped) |> 
    dplyr::group_by(Region, BasinName, SubBasin, EnvValueZone) |> 
    dplyr::summarise(geom = sf::st_union(geom))

  #cut the extent back using the surrounding polygons from land and marine
  dt_sb_grouped <- dt_sb_grouped |> 
    sf::st_difference(sf::st_union(dplyr::filter(basins, BasinName != "Black", BasinName != "Ross"))) |> 
    sf::st_difference(sf::st_union(n3_marine))  

  #sub out the old Ross and black basins from the land object, and bind in the new version we just made
  n3_land <- n3_land |> 
    dplyr::mutate(SubBasin = BasinName, EnvValueZone = NA) |> 
    dplyr::filter(BasinName != "Black", BasinName != "Ross") |> 
    rbind(dt_sb_grouped)

  #update the unique tracker for the islands
  n3_land <- n3_land |> 
    dplyr::mutate(BasinName = dplyr::case_when(
      BasinName == "Palm Islands" ~ "Black",
      BasinName == "Magnetic Island" ~ "Ross",
      TRUE ~ BasinName)) 
   
  #---------------
  # Finish DT SUB BASINS, START BURDEKIN SUB BASINS
  #---------------

  #load in epp dataset from file
  #epp_water_schedule <- sf::st_read("epp_water_schedule.gpkg") |> 
  #  name_cleaning() |> 
  #  sf::st_transform("EPSG:7855")

  #load in epp datasets from file
  #epp_water_env_value <- sf::st_read("epp_water_env_value.gpkg") |> 
  #  name_cleaning() |> 
  #  sf::st_transform("EPSG:7855")

  #bd_sub_basins <- epp_water_schedule |>
  #  dplyr::filter(stringr::str_detect(BasinName, "Burdekin|Haughton")) |> 
  #  dplyr::mutate(
  #    Region = "Burdekin",
  #    BasinName = dplyr::case_when(stringr::str_detect(BasinName, "Burdekin") ~ "Burdekin", TRUE ~ "Haughton"),
  #    SubBasin = dplyr::case_when(
  #      PlanId == "WQ1201" ~ "Upper Burdekin",
  #      PlanId == "WQ1202" ~ "Cape Campaspe",
  #      PlanId == "WQ1203" ~ "Belyando",
  #      PlanId == "WQ1204" ~ "Suttor",
  #      PlanId == "WQ1205" ~ "Broke Bogie",
  #      PlanId == "WQ1206" ~ "Lower Burdekin",
  #      PlanId == "WQ1191" ~ "Haughton")) |> 
  #  dplyr::select(Region, BasinName, SubBasin)

  #bd_env_zones <- epp_water_env_value |> 
  #  dplyr::filter(stringr::str_detect(BasinName, "Burdekin|Haughton")) |> 
  #  dplyr::mutate(EnvValueZone = dplyr::case_when(
  #    stringr::str_detect(EnvValueZone, "Burdekin River \\(above dam\\)$") ~ "Burdekin River (Above Dam) - Outside DTA",
  #    stringr::str_detect(EnvValueZone, "Haughton River$") ~ "Haughton River - Outside DTA",
  #    stringr::str_detect(EnvValueZone, "Star River - Defence training area") ~ "Star River - Defence Training Area",
  #    stringr::str_detect(EnvValueZone, "Star River$") ~ "Star River - Outside DTA",
  #    TRUE ~ EnvValueZone)) |>
  #  dplyr::group_by(EnvValueZone) |> 
  #  dplyr::summarise(geom = sf::st_union(geom)) |> 
  #  dplyr::rename(WatercourseOrGeographicArea = EnvValueZone)

  #bd_sub_basins <- sf::st_intersection(bd_sub_basins, bd_env_zones) |> 
  #  sf::st_collection_extract("POLYGON") |> 
  #  dplyr::group_by(Region, BasinName, SubBasin, WatercourseOrGeographicArea) |> 
  #  dplyr::summarise(geom = sf::st_union(geom))

  #load in the burdekin sub basins (above is how this file was originally made)
  load(system.file("extdata/bd_sub_basins.RData", package = "RcTools"))

  #make each instance of watercourse completely unique and cut the data back by the DT area (around alligator creek) and marine area
  bd_sub_basins <- bd_sub_basins |> 
    dplyr::group_by(WatercourseOrGeographicArea) |> 
    dplyr::mutate(Rows = dplyr::n()) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(NewSubBasin = glue::glue("({SubBasin})")) |> 
    tidyr::unite(Combined, WatercourseOrGeographicArea, NewSubBasin, sep = " ", remove = FALSE) |> 
    dplyr::mutate(WatercourseOrGeographicArea = dplyr::case_when(Rows != 1 ~ Combined, TRUE ~ WatercourseOrGeographicArea)) |> 
    dplyr::select(Region, BasinName, SubBasin, WatercourseOrGeographicArea, geom) |> 
    sf::st_difference(sf::st_union(dt_sb_grouped)) |> 
    sf::st_difference(sf::st_union(n3_marine))

  #sub out the old Burdekin Basin and add in the extra geographic area column
  n3_land <- n3_land |> 
    dplyr::filter(Region != "Burdekin") |> 
    dplyr::rename("WatercourseOrGeographicArea" = EnvValueZone) |> 
    dplyr::bind_rows(bd_sub_basins)

  return(n3_land)

}