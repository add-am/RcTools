#' Build the Marine Zone Boundaries Within the Northern Three Region
#'
#' @param waterbodies An sf object produced by the [build_waterbodies()] function
#'
#' @returns An sf object
#'
#' @examples
#' n3_marine <- build_waterbody_boundaries(waterbodies)
#' 
build_waterbody_boundaries <- function(waterbodies) {

  #create marine boundary lines between each region
  borders <- list(
    wt_north_border = data.frame("lon" = c(321165.9, 331652.0, 401072.3), "lat" = c(8247421, 8247502, 8319886)),
    wt_dt_border = data.frame("lon" = c(422697.9, 439338.4, 439164.2, 521938.1), "lat" = c(7910298, 7910360, 7964522, 8051191)),
    dt_south_border = data.frame("lon" = c(502281.3, 551023.8, 637284.2), "lat" = c(7877884, 7887019, 7976710)),
    burdekin_mwi_border = data.frame("lon" = c(578942.0, 578857.7, 691035.5), "lat" = c(7820799, 7820868, 7948147)),
    mwi_south_border = data.frame(
      "lon" = c(764088.4, 764081.9, 779881.7, 784942.7, 813398.1, 820906.8, 829300.4, 952892.5),
      "lat" = c(7545087, 7544699, 7556270, 7570574, 7587948, 7647154, 7647032, 7772419))
    )
  
  #extract geometries as polygons
  waterbodies <- sf::st_collection_extract(waterbodies, "POLYGON")

  #convert each border into a LINESTRING in EPSG:7855 
  border_lines <- lapply(borders, function(df) {sf::st_linestring(as.matrix(df)) |> sf::st_sfc(crs = 7855)}) 
  
  #combine all LINESTRING geometries into one sfc object 
  all_borders <- do.call(c, border_lines)

  #increase the minimum precision of each object
  waterbodies <- sf::st_set_precision(waterbodies, 100) 
  all_borders <- sf::st_set_precision(all_borders, 100)
  
  #split marine area and create a point geom for each of the polygons 
  split_regions <- lwgeom::st_split(waterbodies, all_borders) |> 
    sf::st_collection_extract("POLYGON") |> 
    sf::st_sf() |> 
    dplyr::mutate(geom2 = sf::st_centroid(sf::st_geometry(geom)))
  
  #assign region based on where each polygon is located. This is a trial and error process to eliminate errors
  n3_marine <- split_regions |> 
    dplyr::mutate(Region = dplyr::case_when(
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < 8248068.5 & sf::st_coordinates(geom2)[,2] > 7911391.0 ~ "Wet Tropics",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < 7910384.1 & sf::st_coordinates(geom2)[,2] > 7865492.4 & sf::st_coordinates(geom2)[,1] < 502438.2 ~ "Dry Tropics",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < 7877819.3 & sf::st_coordinates(geom2)[,2] > 7805427.3 &
        sf::st_coordinates(geom2)[,1] < 578160.8 & sf::st_coordinates(geom2)[,1] > 502438.2 ~ "Burdekin",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < 7804475.6 & sf::st_coordinates(geom2)[,2] > 7553363.1 ~ "Mackay Whitsunday Isaac",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < 7821849.3 & sf::st_coordinates(geom2)[,2] > 7805427.3 & sf::st_coordinates(geom2)[,1] > 578160.8 ~ "Mackay Whitsunday Isaac",
      stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < 8257393.4 & sf::st_coordinates(geom2)[,2] > 7911391.0 ~ "Wet Tropics",
      stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < 7911391.0 & sf::st_coordinates(geom2)[,2] > 7879047.6 ~ "Dry Tropics",
      stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < 7879047.6 & sf::st_coordinates(geom2)[,2] > 7851903.9 ~ "Burdekin",
      stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < 7851903.9 & sf::st_coordinates(geom2)[,2] > 7567033.3 ~ "Mackay Whitsunday Isaac",
      stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < 8257393.4 & sf::st_coordinates(geom2)[,2] > 7972921.6 ~ "Wet Tropics",
      stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < 7972921.6 & sf::st_coordinates(geom2)[,2] > 7891429.6 ~ "Dry Tropics",
      stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < 7891429.6 & sf::st_coordinates(geom2)[,2] > 7837706.6 ~ "Burdekin",
      stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < 7837706.6 & sf::st_coordinates(geom2)[,2] > 7590941.8 ~ "Mackay Whitsunday Isaac",
      stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < 8320033.4 & sf::st_coordinates(geom2)[,2] > 8032423.9 ~ "Wet Tropics",
      stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < 8032423.9 & sf::st_coordinates(geom2)[,2] > 7956756.4 ~ "Dry Tropics",
      stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < 7956756.4 & sf::st_coordinates(geom2)[,2] > 7911391.0 ~ "Burdekin",
      stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < 7911391.0 & sf::st_coordinates(geom2)[,2] > 7650338.5 ~ "Mackay Whitsunday Isaac")) |>
    dplyr::filter(!is.na(Region)) |> 
    dplyr::mutate(SubZone = dplyr::case_when(
      stringr::str_detect(SubZone, "Enc") ~ "Enclosed Coastal",
      stringr::str_detect(SubZone, "Ope") ~ "Open Coastal",
      TRUE ~ SubZone)) |> 
    dplyr::select(-geom2) |> 
    sf::st_make_valid()

  #create lines between each zone
  borders2 <- list(
    halifax_cleveland <- data.frame("lon" = c(465591.9, 491195.6), "lat" = c(7878308, 7939939)),
    north_whit <- data.frame("lon" = c(653872.0, 666098.3), "lat" = c(7780032, 7797153)),
    whit_central <- data.frame("lon" = c(700962.2, 735055.9), "lat" = c(7728215, 7722336)),
    central_south <- data.frame("lon" = c(757235.0, 800067.5), "lat" = c(7616770, 7646718)),
    wt_north_central <- data.frame("lon" = c(384810.1, 401881.1), "lat" = c(8135152, 8146734)),
    wt_central_south <- data.frame("lon" = c(409699.6, 432633.6), "lat" = c(8048488, 8048578)),
    south_palm <- data.frame("lon" = c(422201.3, 439197.2, 439164.2, 453671.4), "lat" = c(7954104, 7954166, 7964522, 7979798))
  )

  #convert each border into a LINESTRING in EPSG:7855 
  border_lines2 <- lapply(borders2, function(df) {sf::st_linestring(as.matrix(df)) |> sf::st_sfc(crs = 7855)}) 
  
  #combine all LINESTRING geometries into one sfc object 
  all_borders2 <- do.call(c, border_lines2)

  #increase the minimum precision of each object
  waterbodies <- sf::st_set_precision(waterbodies, 100) 
  all_borders2 <- sf::st_set_precision(all_borders2, 100)

  #split marine area and create a point geom for each of the polygons 
  split_zones <- lwgeom::st_split(n3_marine, all_borders2) |> 
    sf::st_collection_extract("POLYGON") |> 
    sf::st_sf() |> 
    dplyr::mutate(
      geom2 = sf::st_centroid(geom),
      Zone = SubZone
    )  

  #assign zone information based on where each polygon is located: this is done in stages. do wet tropics first
  wt_marine <- split_zones |> 
    dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet Tropics") ~ "Northern", TRUE ~ Zone)) |> 
    dplyr::mutate(Zone = dplyr::case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet Tropics") & sf::st_coordinates(geom2)[,1] > 384862.6 & sf::st_coordinates(geom2)[,2] < 8135153 ~ "Central", 
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet Tropics") & sf::st_coordinates(geom2)[,1] > 379320 & sf::st_coordinates(geom2)[,2] < 8127436 ~ "Central", TRUE ~ Zone)) |> 
    dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,2] < 8048663 ~ "Southern", TRUE ~ Zone)) |> 
    dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") &  stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,2] < 7954262 ~ "Palm Group", TRUE ~ Zone)) 
    
  #then do dry tropics
  dt_marine <- wt_marine |>
    dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Dry") ~ "Halifax Bay", TRUE ~ Zone)) |> 
    dplyr::mutate(Zone = dplyr::case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Dry") & sf::st_coordinates(geom2)[,1] > 466064.8 & 
        sf::st_coordinates(geom2)[,2] < 7937544 ~ "Cleveland Bay", TRUE ~ Zone))

  #next is burdekin
  bd_marine <- dt_marine |>  
    dplyr::mutate(Zone = dplyr::case_when(
        stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Burd") & sf::st_coordinates(geom2)[,1] > 502288.7 & 
          sf::st_coordinates(geom2)[,2] < 7877869~ "Burdekin",TRUE ~ Zone))

  #and finally mackay
  all_marine <- bd_marine |> 
    dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open") & stringr::str_detect(Region, "Mackay") ~ "North", TRUE ~ Zone)) |> 
    dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open") & sf::st_coordinates(geom2)[,1] > 655274.4 & sf::st_coordinates(geom2)[,2] < 7788874 ~ "Whitsunday", TRUE ~ Zone)) |> 
    dplyr::mutate(Zone = dplyr::case_when(
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] < 693987.2 & sf::st_coordinates(geom2)[,2] < 7738799 ~ "Central",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] < 698673.9 & sf::st_coordinates(geom2)[,2] < 7733296 ~ "Central",
      stringr::str_detect(SubZone, "Enc|Open") & sf::st_coordinates(geom2)[,2] < 7729880 ~ "Central", TRUE ~ Zone)) |> 
    dplyr::mutate(Zone = dplyr::case_when(
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] > 759883.2 & sf::st_coordinates(geom2)[,2] < 7618823 ~ "South",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] > 756098.2 & sf::st_coordinates(geom2)[,2] < 7615067 ~ "South",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < 7612584 ~ "South",
      stringr::str_detect(SubZone, "Open") & sf::st_coordinates(geom2)[,1] > 759883.2 & sf::st_coordinates(geom2)[,2] < 7639843 ~ "South", TRUE ~ Zone))
  
  #remove old geom and add the Environment type
  n3_marine <- dplyr::select(all_marine, -geom2) |> 
    dplyr::mutate(Environment = "Marine")
 
  return(n3_marine)

}