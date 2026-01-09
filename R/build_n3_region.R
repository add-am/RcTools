build_n3_region <- function(){


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
  basins_extract <- arcgislayers::arc_select(basins, where = basin_query)
  subs_extract <- arcgislayers::arc_select(subs, where = sub_query)

  #clean data and combine
  basin_and_sub <- subs_extract |> 
    dplyr::rename("basin_name" = "sub_name", "basin_number" = "sub_number") |> 
    rbind(basins_extract) |> 
    sf::st_transform("EPSG:7844")
  
  #bring in all other islands (that each have their own polygon), and join the two datasets
  named_islands <- get(data("gbr_feat", package = "gisaimsr")) |> 
    dplyr::filter(FEAT_NAME == "Island" & GBR_NAME != "U/N Island")|> 
    dplyr::mutate(geometry2 = sf::st_centroid(geometry)) |> 
    dplyr::select(OBJECTID, geometry, geometry2)
    
  #referencing the point geom, categorize each of the islands into a specific region
  named_islands <- named_islands |> 
      dplyr::mutate(
        Region = dplyr::case_when(
          sf::st_coordinates(geometry2)[,2] > -18.54 ~ "Wet Tropics",
          sf::st_coordinates(geometry2)[,2] < -18.54 & sf::st_coordinates(geometry2)[,2] > -19.3 & sf::st_coordinates(geometry2)[,1] > 146.36157 ~ "Dry Tropics",
          sf::st_coordinates(geometry2)[,2] < -19.72 & sf::st_coordinates(geometry2)[,2] > -22.13 ~ "Mackay Whitsunday Isaac")) |> 
    tidyr::drop_na(Region)

  #remove a few instances that overlap with the mainland
  named_islands <- dplyr::filter(named_islands, !OBJECTID %in% c(606, 539, 4960))

  #Create a integer of the nearest basin to each island
  nearest_basin <- sf::st_nearest_feature(named_islands, basin_and_sub)

  #create a matching system for the integers
  replace_with <- unique(basin_and_sub$basin_name)
  replace_from <- c(1:length(unique(basin_and_sub$basin_name)))

  #include which basin each island belongs to
  named_islands$basin_name <- c(replace_with, nearest_basin)[match(nearest_basin, c(replace_from, nearest_basin))]

  #determined the closest basin, and override the palm island group to give them to the dry tropics, also add a unique names for dt islands
  named_islands <- named_islands |> 
    dplyr::mutate(
      basin_name = c(replace_with, nearest_basin)[match(nearest_basin, c(replace_from, nearest_basin))],
      basin_name = dplyr::case_when(
        Region == "Dry Tropics" & basin_name == "Herbert" ~ "Palm Islands",
        Region == "Dry Tropics" & basin_name == "Black" ~ "Palm Islands",
        basin_name == "Ross" ~ "Magnetic Island",
        TRUE ~ basin_name)
      )
  
  #keep only islands within the bounding box of the land basins
  basin_extent <- sf::st_bbox(basin_and_sub)
  basin_extent[3] <- 155.0000
  named_islands <- sf::st_crop(named_islands, basin_extent)  
  
  #read the water bodies data set from the aims package
  water_bodies <- get(data("wbodies", package = "gisaimsr"))

  #update crs
  water_bodies <- sf::st_transform(water_bodies, "EPSG:7855")
  basin_and_sub <- sf::st_transform(basin_and_sub, "EPSG:7855")
  named_islands <- sf::st_transform(named_islands, "EPSG:7855")

  #create a buffer around the water bodies then take only the buffer, union everything as no info is needed currently
  wb_buffer_dif <- water_bodies |> 
    dplyr::mutate(geometry = sf::st_buffer(geometry, dist = 4000)) |> 
    sf::st_difference(sf::st_union(water_bodies)) |> 
    sf::st_union() |> 
    sf::st_as_sf()

  #create a buffer around the land basins then take only the buffer, union everything as no info is needed currently
  basin_buffer_dif <- basin_and_sub |> 
    dplyr::mutate(geometry = sf::st_buffer(geometry, dist = 4000)) |> 
    sf::st_difference(sf::st_union(basin_and_sub)) |> 
    sf::st_union() |> 
    sf::st_as_sf()

  #get the area that is shared between the two  zones (this creates a fill layer between the og basin and wb datasets), add info
  area_shared <- sf::st_intersection(basin_buffer_dif, wb_buffer_dif) |> 
    dplyr::mutate(SubZone = "Enclosed Coastal") |> dplyr::rename(geometry = x)

  #join the shared area onto the water bodies data. This now fills all gaps between water and land, but has not removed overlaps between water and land
  water_bodies_expanded <- water_bodies |> 
    dplyr::select(MarineWate) |> 
    dplyr::rename(SubZone = MarineWate) |> 
    rbind(area_shared) 

  #make everything valid
  water_bodies_expanded <- sf::st_make_valid(water_bodies_expanded)
  basin_and_sub <- sf::st_make_valid(basin_and_sub)
  named_islands <- sf::st_make_valid(named_islands)

  #union the two "subtract" layers into one geometry
  remove_geom <- sf::st_union(sf::st_union(basin_and_sub), sf::st_union(named_islands))

  #pre-filter water bodies that actually intersect the removal geometry
  idx <- sf::st_intersects(water_bodies_expanded, remove_geom) 
  intersecting <- water_bodies_expanded[lengths(idx) > 0, ] 
  non_intersecting <- water_bodies_expanded[lengths(idx) == 0, ]

  #convert to terra for faster processing
  inter_v <- terra::vect(intersecting)
  rem_v <- terra::vect(remove_geom)
  inter_final_v <- terra::erase(inter_v, rem_v)

  #then convert back
  inter_final <- sf::st_as_sf(inter_final_v)

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
    dplyr::summarise(geometry = sf::st_union(geometry))

  #---------------
  # CHECK POINT 1: COMPLETE
  #---------------
  tmap::tm_shape(water_bodies_final) + tmap::tm_polygons(fill = "SubZone", fill_alpha = 1) 
  #---------------

  #create marine boundary lines between each region
  borders <- list(
    wt_north_border = data.frame("lon" = c(145.3300, 145.4279, 146.0790), "lat" = c(-15.8457, -15.8457, -15.1951)),
    wt_dt_border = data.frame("lon" = c(146.2660, 146.4240, 146.4240, 147.2068), "lat" = c(-18.8980, -18.8980,-18.4085, -17.6259)),
    dt_south_border = data.frame("lon" = c(147.0217, 147.4851, 148.2989), "lat" = c(-19.1924, -19.1092, -18.2948)),
    burdekin_mwi_border = data.frame("lon" = c(147.7532638, 147.7524562, 148.81000), "lat" = c(-19.7067070, -19.7060907, -18.54870)),
    mwi_south_border = data.frame(
      "lon" = c(149.5611, 149.5611, 149.7122, 149.7587, 150.0306, 150.0918, 150.1726, 151.3289),
      "lat" = c(-22.1794, -22.1829, -22.0760, -21.9461, -21.7845, -21.2490, -21.2486, -20.0923))
    )
  
  #extract geometries as polygons
  water_bodies_final <- sf::st_collection_extract(water_bodies_final, "POLYGON")

  # 1. Convert each border into a LINESTRING in EPSG:7855 
  border_lines <- lapply(borders, function(df) { 
    st_linestring(as.matrix(df)) |> st_sfc(crs = 4326) |> 
      st_transform("EPSG:7855") }) 
  
  # 2. Combine all LINESTRING geometries into one sfc object 
  all_borders <- do.call(c, border_lines)

   # 2. Split marine area 
  marine_area <- water_bodies_final#st_union(water_bodies_final) 
  
  split_regions <- lwgeom::st_split(marine_area, all_borders) |> 
    st_collection_extract("POLYGON") |> 
    st_sf()

  #add a point geom for each of the polygons 
split_regions <- split_regions |> 
  sf::st_transform("EPSG:4326") |> 
  dplyr::mutate(geom2 = sf::st_centroid(sf::st_geometry(geometry)))

#assign region based on where each polygon is located. This is a trial and error process to eliminate errors
n3_marine <- split_regions |> 
  sf::st_transform("EPSG:4326") |> 
  dplyr::mutate(Region = case_when(
    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -15.8457 & sf::st_coordinates(geom2)[,2] > -18.8980 ~ "Wet Tropics",
    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -18.8980 & sf::st_coordinates(geom2)[,2] > -19.3037 & sf::st_coordinates(geom2)[,1] < 147.0235 ~ "Dry Tropics",
    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -19.1923 & sf::st_coordinates(geom2)[,2] > -19.8465 &
      sf::st_coordinates(geom2)[,1] < 147.7533 & sf::st_coordinates(geom2)[,1] > 147.0213 ~ "Burdekin",
    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -19.8551 & sf::st_coordinates(geom2)[,2] > -22.1240 ~ "Mackay Whitsunday Isaac",
    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -19.6981 & sf::st_coordinates(geom2)[,2] > -19.8551 & sf::st_coordinates(geom2)[,1] > 147.7482 ~ "Mackay Whitsunday Isaac",
    stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < -15.7614 & sf::st_coordinates(geom2)[,2] > -18.8980 ~ "Wet Tropics",
    stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < -18.8980 & sf::st_coordinates(geom2)[,2] > -19.1812 ~ "Dry Tropics",
    stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < -19.1812 & sf::st_coordinates(geom2)[,2] > -19.4265 ~ "Burdekin",
    stringr::str_detect(SubZone, "Ope") & sf::st_coordinates(geom2)[,2] < -19.4265 & sf::st_coordinates(geom2)[,2] > -22.0005 ~ "Mackay Whitsunday Isaac",
    stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < -15.7614 & sf::st_coordinates(geom2)[,2] > -18.3328 ~ "Wet Tropics",
    stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < -18.3328 & sf::st_coordinates(geom2)[,2] > -19.0693 ~ "Dry Tropics",
    stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < -19.0693 & sf::st_coordinates(geom2)[,2] > -19.5548 ~ "Burdekin",
    stringr::str_detect(SubZone, "Mid") & sf::st_coordinates(geom2)[,2] < -19.5548 & sf::st_coordinates(geom2)[,2] > -21.7845 ~ "Mackay Whitsunday Isaac",
    stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < -15.1951 & sf::st_coordinates(geom2)[,2] > -17.7950 ~ "Wet Tropics",
    stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < -17.7950 & sf::st_coordinates(geom2)[,2] > -18.4789 ~ "Dry Tropics",
    stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < -18.4789 & sf::st_coordinates(geom2)[,2] > -18.8889 ~ "Burdekin",
    stringr::str_detect(SubZone, "Off") & sf::st_coordinates(geom2)[,2] < -18.8889 & sf::st_coordinates(geom2)[,2] > -21.2486 ~ "Mackay Whitsunday Isaac")) |>
  dplyr::filter(!is.na(Region)) |> 
  dplyr::mutate(SubZone = case_when(
    stringr::str_detect(SubZone, "Enc") ~ "Enclosed Coastal",
    stringr::str_detect(SubZone, "Ope") ~ "Open Coastal",
    TRUE ~ SubZone)) |> 
  dplyr::select(-geom2)
  
  #---------------
  # CHECK POINT 2: COMPLETE
  #---------------
  tmap::tm_shape(n3_marine) + tmap::tm_polygons(fill = "Region", fill_alpha = 1) 
  #---------------
    
  #create lines between each region
  borders2 <- list(
    halifax_cleveland <- data.frame("lon" = c(146.675117968, 146.91653), "lat" = c(-19.189224827, -18.63154)),
    north_whit <- data.frame("lon" = c(148.47151, 148.58685), "lat" = c(-20.07056, -19.91489)),
    whit_central <- data.frame("lon" = c(148.927459, 149.25506), "lat" = c(-20.534212, -20.58335)),
    central_south <- data.frame("lon" = c(149.483468, 149.8913), "lat" = c(-21.533408, -21.2565)),
    wt_north_central <- data.frame("lon" = c(145.918657, 146.0794), "lat" = c(-16.864201, -16.7603)),
    wt_central_south <- data.frame("lon" = c(146.1487, 146.3649), "lat" = c(-17.6486, -17.6486)),
    south_palm <- data.frame("lon" = c(146.2630, 146.4240, 146.4240, 146.5617), "lat" = c(-18.5021, -18.5021, -18.4085, -18.2708))
  )

  #extract geometries as polygons
  n3_marine <- sf::st_collection_extract(n3_marine, "POLYGON") |> 
    st_transform("EPSG:7855")

  # 1. Convert each border into a LINESTRING in EPSG:7855 
  border_lines <- lapply(borders2, function(df) { 
    st_linestring(as.matrix(df)) |> st_sfc(crs = 4326) |> 
      st_transform("EPSG:7855") }) 
  
  # 2. Combine all LINESTRING geometries into one sfc object 
  all_borders <- do.call(c, border_lines)

   # 2. Split marine area 
  marine_area <- n3_marine#st_union(water_bodies_final) 
  
  split_regions <- lwgeom::st_split(marine_area, all_borders) |> 
    st_collection_extract("POLYGON") |> 
    st_sf()

  n3_marine <- split_regions

  #---------------
  # CHECK POINT 4: COMPLETE
  #---------------
  tmap::tm_shape(n3_marine) + tmap::tm_polygons(fill = "Region", fill_alpha = 1) 
  backup <- n3_marine
  #---------------

  library(dplyr) 
  library(stringr) 
  library(sf) 
  

  #add a point geom for each of the polygons 
n3_marine$geom2 <- st_centroid(st_geometry(n3_marine))

#add preliminary zone information for those which don't have special names
n3_marine <- n3_marine |> mutate(Zone = SubZone)

#assign region based on where each polygon is located
n3_marine <- n3_marine |> 
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet") ~ "Northern", 
      TRUE ~ Zone)) |> 
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,1] > 145.91915 & sf::st_coordinates(geom2)[,2] < -16.8642 ~ "Central", 
      TRUE ~ Zone)) |> 
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,1] > 145.86671 & sf::st_coordinates(geom2)[,2] < -16.93366 ~ "Central", 
      TRUE ~ Zone)) |> 
  dplyr::mutate(
    Zone = case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,2] < -17.6486 ~ "Southern", 
    TRUE ~ Zone)) |>
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") &  stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,2] < -18.5021 ~ "Palm Group", 
      TRUE ~ Zone)) |>
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Dry") ~ "Halifax Bay", 
      TRUE ~ Zone)) |> 
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Dry") & sf::st_coordinates(geom2)[,1] > 146.68007 & sf::st_coordinates(geom2)[,2] < -18.6532 ~ "Cleveland Bay", 
      TRUE ~ Zone)) |> 
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Burd") & sf::st_coordinates(geom2)[,1] > 147.0217007 & sf::st_coordinates(geom2)[,2] > -19.206464 & sf::st_coordinates(geom2)[,2] < -19.1924341 ~ "Burdekin",
      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Burd") & sf::st_coordinates(geom2)[,1] > 147.024120 & sf::st_coordinates(geom2)[,2] < -19.208543 ~ "Burdekin", 
      TRUE ~ Zone)) |> 
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc") & stringr::str_detect(Region, "Mackay") ~ "North",
      stringr::str_detect(SubZone, "Open") & stringr::str_detect(Region, "Mackay") ~ "North",
      TRUE ~ Zone)) |>
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] > 148.476995 & sf::st_coordinates(geom2)[,2] < -19.97939 ~ "Whitsunday", 
      stringr::str_detect(SubZone, "Open") & sf::st_coordinates(geom2)[,1] > 148.476995 & sf::st_coordinates(geom2)[,2] < -19.97939 ~ "Whitsunday", 
      TRUE ~ Zone)) |>
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] < 148.844991 & sf::st_coordinates(geom2)[,2] < -20.431976 ~ "Central",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] < 148.889737 & sf::st_coordinates(geom2)[,2] < -20.495653 ~ "Central",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -20.52959 ~ "Central",
      stringr::str_detect(SubZone, "Open") & sf::st_coordinates(geom2)[,2] < -20.52622 ~ "Central", 
      TRUE ~ Zone)) |> 
  dplyr::mutate(
    Zone = case_when(
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] > 149.483495 & sf::st_coordinates(geom2)[,2] < -21.533348 ~ "South",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] > 149.45443 & sf::st_coordinates(geom2)[,2] < -21.56729 ~ "South",
      stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -21.583174 ~ "South",
      stringr::str_detect(SubZone, "Open") & sf::st_coordinates(geom2)[,1] > 149.483495 & sf::st_coordinates(geom2)[,2] < -21.33330 ~ "South", 
      TRUE ~ Zone))

#remove old geom and add the Environment type
n3_marine <- select(n3_marine, -geom2) |> mutate(Environment = "Marine")


  #---------------
  # CHECK POINT 5: COMPLETE
  #---------------
  tmap::tm_shape(n3_marine) + tmap::tm_polygons(fill = "Zone", fill_alpha = 1) 
  #---------------
  
  
#tmap_mode("view")
tmap::tm_shape(n3_marine) +
  tmap::tm_polygons(fill = "SubZone")
  


  tmap::tm_shape(split_regions) + tmap::tm_polygons(fill = "red", fill_alpha = 1) 


  
  #tmap::tmap_mode("view")
  
  tmap::tm_shape(water_bodies_final) + tmap::tm_polygons(fill = "red", fill_alpha = 0.5) +
    tmap::tm_shape(water_bodies) + tmap::tm_polygons(fill = "green", fill_alpha = 0.5) +
    tmap::tm_shape(basin_and_sub) + tmap::tm_polygons(fill = "blue", fill_alpha = 0.5) +
    tmap::tm_shape(named_islands) + tmap::tm_polygons(fill = "orange", fill_alpha = 0.5) +
    tmap::tm_shape(area_shared) + tmap::tm_polygons(fill = "purple", fill_alpha = 0.5)

  
    







}