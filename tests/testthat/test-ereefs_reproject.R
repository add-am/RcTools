test_that("ereefs_reproject returns a regular stars object (single attribute)", { 
  
  #load the target file
  load(system.file("extdata/single_att_curvi.RData", package = "RcTools"))
  
  #run the reproject function
  single_att_reg <- suppressMessages(ereefs_reproject(single_att_curvi))

  #check the function returns a stars object, that the projection is regular, and that the projection changed
  expect_s3_class(single_att_reg, "stars")
  expect_equal(stars::st_raster_type(single_att_reg), "regular") 
  expect_true(sf::st_crs(single_att_reg) != sf::st_crs(single_att_curvi))

})

test_that("ereefs_reproject returns a regular stars object (multi-attribute)", { 
  
  #load the target file
  load(system.file("extdata/multi_att_curvi.RData", package = "RcTools"))
  
  #run the reproject function
  multi_att_reg <- suppressMessages(ereefs_reproject(multi_att_curvi))

  #check the function returns a stars object, that the projection is regular, and that the projection changed
  expect_s3_class(multi_att_reg, "stars")
  expect_equal(stars::st_raster_type(multi_att_reg), "regular") 
  expect_true(sf::st_crs(multi_att_reg) != sf::st_crs(multi_att_curvi))

})
