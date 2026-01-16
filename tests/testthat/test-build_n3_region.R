test_that("build_n3_region returns an sf object", {
  
  #run the function
  basins <- suppressWarnings(build_basins())

  #check type of object
  expect_s3_class(basins, "sf")

  #check geometry type
  expect_equal(unique(as.character(sf::st_geometry_type(basins))), "POLYGON")

  #check dimensions
  expect_equal(dim(basins), c(18, 6))

  #check column names
  expect_contains(colnames(basins), c("Objectid", "BasinName", "BasinNumber", "Globalid", "geom", "Region"))
  
  #------------------------------------------------------------------------------------------------------
  
  #run the function
  named_islands <- suppressWarnings(build_named_islands(basins))

  #check type of object
  expect_s3_class(named_islands, "sf")

  #check geometry type
  expect_contains(unique(as.character(sf::st_geometry_type(named_islands))), c("POLYGON", "MULTIPOLYGON"))

  #check dimensions
  expect_equal(dim(named_islands), c(333, 3))

  #check column names
  expect_contains(colnames(named_islands), c("Region", "BasinName", "geom"))
    
  #------------------------------------------------------------------------------------------------------

  #run the function
  waterbodies <- suppressWarnings(build_waterbodies(basins, named_islands))

  #check type of object
  expect_s3_class(waterbodies, "sf")

  #check geometry type
  expect_contains(unique(as.character(sf::st_geometry_type(waterbodies))), c("POLYGON", "MULTIPOLYGON"))

  #check dimensions
  expect_equal(dim(waterbodies), c(4, 2))

  #check column names
  expect_contains(colnames(waterbodies), c("SubZone", "geom"))

  #------------------------------------------------------------------------------------------------------

  #run the function
  n3_marine <- suppressWarnings(build_waterbody_boundaries(waterbodies))

  #check type of object
  expect_s3_class(n3_marine, "sf")

  #check geometry type
  expect_equal(unique(as.character(sf::st_geometry_type(n3_marine))), "POLYGON")

  #check dimensions
  expect_equal(dim(n3_marine), c(592, 5))

  #check column names
  expect_contains(colnames(n3_marine), c("SubZone", "Region", "geom", "Zone", "Environment"))
 
  #------------------------------------------------------------------------------------------------------

  #combine basins and islands
  n3_land <- basins |> 
    dplyr::select(Region, BasinName) |> 
    rbind(named_islands)
  
  #------------------------------------------------------------------------------------------------------  

  #run the function
  n3_land <- suppressWarnings(build_sub_basins(n3_land, basins, n3_marine))

  #check type of object
  expect_s3_class(n3_land, "sf")

  #check geometry type
  expect_contains(unique(as.character(sf::st_geometry_type(n3_land))), c("POLYGON", "MULTIPOLYGON"))

  #check dimensions
  expect_equal(dim(n3_land), c(451, 5))

  #check column names
  expect_contains(colnames(n3_land), c("Region", "BasinName", "SubBasin", "WatercourseOrGeographicArea", "geom"))

  #------------------------------------------------------------------------------------------------------
 
  #run the function
  n3_land <- suppressWarnings(build_water_types(n3_land, n3_marine))

  #check type of object
  expect_s3_class(n3_land, "sf")

  #check geometry type
  expect_contains(unique(as.character(sf::st_geometry_type(n3_land))), c("POLYGON", "MULTIPOLYGON"))

  #check dimensions
  expect_equal(dim(n3_land), c(3765, 6))

  #check column names
  expect_contains(colnames(n3_land), c("Environment", "Region", "BasinName", "SubBasin", "WatercourseOrGeographicArea", "geom"))

  #------------------------------------------------------------------------------------------------------
  
  #run the function
  n3_land <- suppressWarnings(build_land_sp(n3_land))

  #check type of object
  expect_s3_class(n3_land, "sf")

  #check geometry type
  expect_contains(unique(as.character(sf::st_geometry_type(n3_land))), c("POLYGON", "MULTIPOLYGON"))

  #check dimensions
  expect_equal(dim(n3_land), c(3766, 6))

  #check column names
  expect_contains(colnames(n3_land), c("Environment", "Region", "BasinName", "SubBasin", "WatercourseOrGeographicArea", "geom"))

  #------------------------------------------------------------------------------------------------------
  
  #run the function
  n3_marine <- suppressWarnings(build_marine_sp(n3_marine, n3_land))

  #check type of object
  expect_s3_class(n3_marine, "sf")

  #check geometry type
  expect_contains(unique(as.character(sf::st_geometry_type(n3_marine))), c("POLYGON", "MULTIPOLYGON"))

  #check dimensions
  expect_equal(dim(n3_marine), c(608, 6))

  #check column names
  expect_contains(colnames(n3_marine), c("SubZone", "Region", "Zone", "Environment", "geom", "WatercourseOrGeographicArea"))

})
