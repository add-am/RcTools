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
    #dplyr::rename("BasinName" = "basin_name") |>
    sf::st_transform("EPSG:7855") |> 
    dplyr::mutate(Region = dplyr::case_when(
      BasinName %in% c("Ross", "Black") ~ "Dry Tropics",
      BasinName %in% c("Burdekin", "Haughton") ~ "Burdekin",
      BasinName %in% c("Plane", "Don", "Proserpine", "Pioneer", "O'Connell") ~ "Mackay Whitsunday Isaac",
      TRUE ~ "Wet Tropics"
    ))
  
  #bring in all other islands (that each have their own polygon), 
  named_islands <- get(data("gbr_feat", package = "gisaimsr")) |> 
    name_cleaning() |> 
    sf::st_transform("EPSG:7855")

  #and join the two datasets
  named_islands <- named_islands |> 
    dplyr::filter(FeatName == "Island" & GbrName != "U/N Island")|> 
    dplyr::mutate(geom2 = sf::st_centroid(geom)) |> 
    dplyr::select(Objectid, geom, geom2)
    
  #referencing the point geom, categorize each of the islands into a specific region
  named_islands <- named_islands |> 
      dplyr::mutate(
        Region = dplyr::case_when(
          sf::st_coordinates(geom2)[,2] > 7949996 ~ "Wet Tropics",
          sf::st_coordinates(geom2)[,2] < 7949996 & sf::st_coordinates(geom2)[,2] > 7865902 & sf::st_coordinates(geom2)[,1] > 447231.6 ~ "Dry Tropics",
          sf::st_coordinates(geom2)[,2] < 7819426 & sf::st_coordinates(geom2)[,2] > 7552699 ~ "Mackay Whitsunday Isaac")) |> 
    tidyr::drop_na(Region)

  #remove a few instances that overlap with the mainland
  named_islands <- dplyr::filter(named_islands, !Objectid %in% c(606, 539, 4960))

  #Create a integer of the nearest basin to each island
  nearest_basin <- sf::st_nearest_feature(named_islands, basin_and_sub)

  #create a matching system for the integers
  replace_with <- unique(basin_and_sub$BasinName)
  replace_from <- c(1:length(unique(basin_and_sub$BasinName)))

  #include which basin each island belongs to
  named_islands$BasinName <- c(replace_with, nearest_basin)[match(nearest_basin, c(replace_from, nearest_basin))]

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
  basin_extent <- sf::st_bbox(basin_and_sub)
  basin_extent[3] <- 155.0000
  named_islands <- sf::st_crop(named_islands, basin_extent)  
  
  #read the water bodies data set from the aims package
  water_bodies <- get(data("wbodies", package = "gisaimsr")) |> 
    name_cleaning() |> 
    sf::st_transform("EPSG:7855")

  #create a buffer around the water bodies then take only the buffer, union everything as no info is needed currently
  wb_buffer_dif <- water_bodies |> 
    dplyr::mutate(geom = sf::st_buffer(geom, dist = 4000)) |> 
    sf::st_difference(sf::st_union(water_bodies)) |> 
    sf::st_union() |> 
    sf::st_as_sf()

  #create a buffer around the land basins then take only the buffer, union everything as no info is needed currently
  basin_buffer_dif <- basin_and_sub |> 
    dplyr::mutate(geom = sf::st_buffer(geom, dist = 4000)) |> 
    sf::st_difference(sf::st_union(basin_and_sub)) |> 
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

  #---------------
  # CHECK POINT 1: COMPLETE
  #---------------
  tmap::tm_shape(water_bodies_final) + tmap::tm_polygons(fill = "SubZone", fill_alpha = 1) 
  #---------------

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
  water_bodies_final <- sf::st_collection_extract(water_bodies_final, "POLYGON")

  #convert each border into a LINESTRING in EPSG:7855 
  border_lines <- lapply(borders, function(df) {sf::st_linestring(as.matrix(df)) |> sf::st_sfc(crs = 7855)}) 
  
  #combine all LINESTRING geometries into one sfc object 
  all_borders <- do.call(c, border_lines)

  #split marine area and create a point geom for each of the polygons 
  split_regions <- lwgeom::st_split(water_bodies_final, all_borders) |> 
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
  
  #---------------
  # CHECK POINT 2: COMPLETE
  #---------------
  tmap::tm_shape(n3_marine) + tmap::tm_polygons(fill = "Region", fill_alpha = 1) 
  #---------------
    
  #create lines between each zone
  borders2 <- list(
    halifax_cleveland <- data.frame("lon" = c(465845.1, 491195.6), "lat" = c(7878093, 7939939)),
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

  #split marine area and create a point geom for each of the polygons 
  split_zones <- lwgeom::st_split(n3_marine, all_borders2) |> 
    sf::st_collection_extract("POLYGON") |> 
    sf::st_sf() |> 
    dplyr::mutate(
      geom2 = st_centroid(geom),
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
  all_marine <- dplyr::select(all_marine, -geom2) |> 
    dplyr::mutate(Environment = "Marine")
 
  #combine islands and land basins  
  n3_land <- rbind(basin_and_sub, named_islands) |> 
    dplyr::select(Region, BasinName)

  #convert to terra for faster processing
  n3_land <- terra::vect(n3_land) 
  all_marine <- terra::vect(all_marine) 

  #cut the marine zone by the land zone
  n3_marine <- terra::erase(all_marine, n3_land)

  #then convert back to sf
  n3_marine <- sf::st_as_sf(n3_marine) |> 
    name_cleaning() 
  n3_land <- sf::st_as_sf(n3_land) |> 
    name_cleaning()

  #---------------
  # CHECK POINT 3: COMPLETE
  #---------------
  tmap::tm_shape(all_marine) + tmap::tm_polygons(fill = "Zone", fill_alpha = 1) 
  #---------------

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
  
  #group up and simplify all geometries (keep the TSDA separate)
  dt_sb_grouped <- dt_sb_grouped |> 
    dplyr::mutate(EnvValueZone = dplyr::case_when(stringr::str_detect(EnvValueZone, "Townsville State Development Area") ~ EnvValueZone, TRUE ~ NA)) |> 
    dplyr::group_by(Region, BasinName, SubBasin, EnvValueZone) |> 
    dplyr::summarise(geom = st_union(geom))

  #---------------
  # CHECK POINT 4: COMPLETE
  #---------------
  tmap::tm_shape(dt_sb_grouped) + tmap::tm_polygons(fill = "SubBasin", fill_alpha = 1) 
  #---------------

  #get the difference between the dt_sub_basin area and the area for ross and black in the n3 files
  diff <- basin_and_sub |> 
    dplyr::filter(BasinName %in% c("Ross", "Black")) |> 
    sf::st_difference(st_union(dt_sb_grouped)) |> 
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
    dplyr::summarise(geom = st_union(geom))

  #cut the extent back using the surrounding polygons
  n3_basin_subtraction <- basin_and_sub |> 
    dplyr::filter(BasinName != "Black", BasinName != "Ross") |> 
    sf::st_union()
    
  n3_water_bodies_subtraction <- sf::st_union(n3_marine)

  #subtract the surrounding polygons
  dt_sb_grouped <- dt_sb_grouped |> 
    sf::st_difference(n3_basin_subtraction) |> 
    sf::st_difference(n3_water_bodies_subtraction)

  #do minor clean up
  dt_sb_grouped <- dt_sb_grouped |> 
    dplyr::filter(
      BasinName != "Black" | BasinName != "Ross River (Upper)",
      BasinName != "Ross" | BasinName != "Black River")

  #sub out the old Ross and black basins
  n3_land <- n3_land |> 
    dplyr::mutate(
      SubBasin = BasinName,
      EnvValueZone = NA
    ) |> 
    dplyr::filter(BasinName != "Black", BasinName != "Ross")

  #bind the data together (i.e. sub in the new dry tropics region)
  n3_land <- rbind(n3_land, dt_sb_grouped)

  #update the unique tracker for the islands
  n3_land <- n3_land |> 
    dplyr::mutate(BasinName = dplyr::case_when(
      BasinName == "Palm Islands" ~ "Black",
      BasinName == "Magnetic Island" ~ "Ross",
      TRUE ~ BasinName)) 

  #---------------
  # CHECK POINT 5: COMPLETE
  #---------------
  tmap::tm_shape(n3_land) + tmap::tm_polygons(fill = "SubBasin", fill_alpha = 1) 
  #---------------

  #load in epp dataset from file
  #epp_water_schedule <- sf::st_read("epp_water_schedule.gpkg") |> 
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

  #make each instance of watercourse completely unique
  test <- bd_sub_basins |> 
    dplyr::group_by(WatercourseOrGeographicArea) |> 
    dplyr::mutate(Rows = dplyr::n()) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(NewSubBasin = glue::glue("({SubBasin})")) |> 
    tidyr::unite(Combined, WatercourseOrGeographicArea, NewSubBasin, sep = " ", remove = FALSE) |> 
    dplyr::mutate(WatercourseOrGeographicArea = dplyr::case_when(Rows != 1 ~ Combined, TRUE ~ WatercourseOrGeographicArea))

  #sub out the old Burdekin Basin and add in the extra geographic area column
  n3_land <- n3_land |> 
    dplyr::filter(Region != "Burdekin") |> 
    dplyr::rename("WatercourseOrGeographicArea" = EnvValueZone) |> 
    dplyr::bind_rows(bd_sub_basins)


  #---------------
  # CHECK POINT 6: COMPLETE
  #---------------
  tmap::tm_shape(n3_land) + tmap::tm_polygons(fill = "WatercourseOrGeographicArea", fill_alpha = 1) 
  #---------------

  #select paluma
  #paluma <- epp_water_env_value |> 
  #  dplyr::filter(EnvValueZone == "Paluma Reservoir") |> 
  #  dplyr::mutate(Region = "Dry Tropics", BasinName = "Black", SubBasin = "Paluma Lake", WatercourseOrGeographicArea = NA) |> 
  #  dplyr::select(Region, BasinName, SubBasin, WatercourseOrGeographicArea, geom)

  #load in the paluma outline (above is how this file was originally made)
  load(system.file("extdata/paluma.RData", package = "RcTools"))

  #cut a hole in the main data
  n3_land <- st_difference(n3_land, st_union(paluma))

  #insert paluma lake
  n3_land <- rbind(n3_land, paluma)

  #---------------
  # CHECK POINT 7: COMPLETE
  #---------------
  tmap::tm_shape(n3_land) + tmap::tm_polygons(fill = "SubBasin", fill_alpha = 1) 
  #---------------

  #load the data in from file
  #epp_water_types <- sf::st_read("epp_water_types.gpkg") |> 
  #  name_cleaning() |> 
  #  sf::st_transform("EPSG:7855")

  #get only the fresh and estuarine watertypes from the EPP data
  #water_types <- epp_water_types |> 
  #  dplyr::filter(!stringr::str_detect(WaterType, "coast|shelf")) |> 
  #  dplyr::mutate(Env = dplyr::case_when(stringr::str_detect(WaterType, "estua") ~  "Estuarine", TRUE ~ "Freshwater")) |> 
  #  dplyr::select(Env, geom)

  #crop the water types to an area only a bit larger than the n3 region and reduce file size for saving
  #n3_water_types <- sf::st_crop(water_types, sf::st_union(n3_land)) |> 
  #  dplyr::group_by(Env) |> 
  #  dplyr::summarise(geom = st_union(geom)) |> 
  #  sf::st_simplify() |> 
  #  nngeo::st_remove_holes() |> 
  #  sf::st_make_valid()

  #load in the water type boundaries (above is how this file was originally made)
  load(system.file("extdata/n3_water_types.RData", package = "RcTools"))


    
  #get the difference between the water types area and the area for the n3 land
  diff <- n3_land |> sf::st_difference(sf::st_union(water_types)) |> sf::st_cast("POLYGON")

  tmap::tm_shape(diff) + tmap::tm_polygons(fill = "Env", fill_alpha = 1) 

  #add a point geom for each of the polygons 
  diff$geom2 <- sf::st_centroid(sf::st_geometry(diff))

  #for each of the centroids figure out which of the water types is closest
  diff$NearestWaterType <- sf::st_nearest_feature(diff, water_types)

  #get the index for each water type and the name of each water type
  replace_with <- water_types$Env
  replace_from <- 1:length(water_types$Env)

  #match each case and replace with the sub basin
  diff$Env <- replace_with[match(diff$NearestWaterType, replace_from)]

  #join the difference back on to the main water type set and union everything up
  n3_water_types <- diff |> dplyr::select(Env, geom) |> rbind(water_types)

  #cut the extent back using n3_polygons
  n3_land_subtraction <- n3_land |> sf::st_union()

  #subtract the surrounding polygons
  n3_water_types <- n3_water_types |> sf::st_intersection(n3_land_subtraction) |> 
    sf::st_collection_extract("POLYGON")

  #intersect each water type over the main dataset
  n3_fresh <- sf::st_intersection(n3_land, sf::st_union(filter(n3_water_types, Env == "Freshwater"))) |> 
    sf::st_collection_extract("POLYGON") |> dplyr::mutate(Environment = "Freshwater")
  n3_estuarine <- st_intersection(n3_land, sf::st_union(filter(n3_water_types, Env == "Estuarine"))) |> 
    sf::st_collection_extract("POLYGON") |> dplyr::mutate(Environment = "Estuarine")

  #join everything together
  n3_land <- rbind(n3_fresh, n3_estuarine) |> sf::st_make_valid()

  #update column names for better clarity
  n3_land <- n3_land |> 
    dplyr::rename(BasinOrZone = Basin, SubBasinOrSubZone = SubBasin) |> 
    nngeo::st_remove_holes() |> 
    sf::st_make_valid() |> 
    dplyr::rename(geom = "geometry") #weirdly this code chunk makes the geom column named geometry?







  
  epp_water_manage <- sf::st_read("epp_water_manage.gpkg") |> 
    name_cleaning() |> 
    sf::st_transform("EPSG:7855")







    
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




#do wet tropics first
#wt_marine <- n3_marine |> 
#  dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet Tropics") ~ "Northern", TRUE ~ Zone)) |> 
#  dplyr::mutate(Zone = dplyr::case_when(
#    stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet Tropics") & sf::st_coordinates(geom2)[,1] > 145.91915 & sf::st_coordinates(geom2)[,2] < -16.8642 ~ "Central", 
#    stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet Tropics") & sf::st_coordinates(geom2)[,1] > 145.86671 & sf::st_coordinates(geom2)[,2] < -16.93366 ~ "Central", TRUE ~ Zone)) |> 
#  dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,2] < -17.6486 ~ "Southern", TRUE ~ Zone)) |> 
#  dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") &  stringr::str_detect(Region, "Wet") & sf::st_coordinates(geom2)[,2] < -18.5021 ~ "Palm Group", TRUE ~ Zone)) 
  
#then do dry tropics
#dt_marine <- wt_marine |>
#  dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Dry") ~ "Halifax Bay", TRUE ~ Zone)) |> 
#  dplyr::mutate(Zone = dplyr::case_when(
#    stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Dry") & sf::st_coordinates(geom2)[,1] > 146.68007 & 
#      sf::st_coordinates(geom2)[,2] < -18.6532 ~ "Cleveland Bay", TRUE ~ Zone))

#next is burdekin
#bd_marine <- dt_marine |>  
#  dplyr::mutate(Zone = dplyr::case_when(
#      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Burd") & sf::st_coordinates(geom2)[,1] > 147.0217007 & 
#        sf::st_coordinates(geom2)[,2] > -19.206464 & sf::st_coordinates(geom2)[,2] < -19.1924341 ~ "Burdekin",
#      stringr::str_detect(SubZone, "Enc|Open|Mid") & stringr::str_detect(Region, "Burd") & sf::st_coordinates(geom2)[,1] > 147.024120 & 
#        sf::st_coordinates(geom2)[,2] < -19.208543 ~ "Burdekin", TRUE ~ Zone))

#and finally mackay
#all_marine <- bd_marine |> 
#  dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open") & stringr::str_detect(Region, "Mackay") ~ "North", TRUE ~ Zone)) |> 
#  dplyr::mutate(Zone = dplyr::case_when(stringr::str_detect(SubZone, "Enc|Open") & sf::st_coordinates(geom2)[,1] > 148.476995 & sf::st_coordinates(geom2)[,2] < -19.97939 ~ "Whitsunday", TRUE ~ Zone)) |> 
#  dplyr::mutate(Zone = dplyr::case_when(
#    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] < 148.844991 & sf::st_coordinates(geom2)[,2] < -20.431976 ~ "Central",
#    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] < 148.889737 & sf::st_coordinates(geom2)[,2] < -20.495653 ~ "Central",
#    stringr::str_detect(SubZone, "Enc|Open") & sf::st_coordinates(geom2)[,2] < -20.52959 ~ "Central", TRUE ~ Zone)) |> 
#  dplyr::mutate(Zone = dplyr::case_when(
#    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] > 149.483495 & sf::st_coordinates(geom2)[,2] < -21.533348 ~ "South",
#    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,1] > 149.45443 & sf::st_coordinates(geom2)[,2] < -21.56729 ~ "South",
#    stringr::str_detect(SubZone, "Enc") & sf::st_coordinates(geom2)[,2] < -21.583174 ~ "South",
#    stringr::str_detect(SubZone, "Open") & sf::st_coordinates(geom2)[,1] > 149.483495 & sf::st_coordinates(geom2)[,2] < -21.33330 ~ "South", TRUE ~ Zone))