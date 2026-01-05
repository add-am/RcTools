test_that("ereefs_reproject returns a regular stars object (multi-attribute)", { 
  
  #load a multi attribute target file
  load(system.file("extdata/tc_curvi.RData", package = "RcTools"))
  
  #run the reproject function
  tc_reg <- suppressMessages(ereefs_reproject(tc_curvi))

  #check that:
  expect_s3_class(tc_reg, "stars") #the function returns a stars object
  expect_equal(stars::st_raster_type(tc_reg), "regular")  #that the projection is regular
  expect_true(sf::st_crs(tc_reg) != sf::st_crs(tc_curvi)) #the projection changed
  expect_contains(sf::st_crs(tc_reg)[[1]], "EPSG:9473") #the crs is the local projected crs
})

test_that("ereefs_reproject returns a regular stars object (single attribute)", { 
  
  #load a single attribute target file
  load(system.file("extdata/turb_curvi.RData", package = "RcTools"))
  
  #run the reproject function
  turb_reg <- suppressMessages(ereefs_reproject(turb_curvi))

  #check that:
  expect_s3_class(turb_reg, "stars") #the function returns a stars object
  expect_equal(stars::st_raster_type(turb_reg), "regular")  #that the projection is regular
  expect_true(sf::st_crs(turb_reg) != sf::st_crs(turb_curvi)) #the projection changed
  expect_contains(sf::st_crs(turb_reg)[[1]], "EPSG:9473") #the crs is the local projected crs

})
