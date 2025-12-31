test_that("ereefs_extract returns a stars object", {

  #load in a small boundary
  sf_obj <- sf::st_read(
      system.file("extdata/boundary.gpkg", package = "RcTools"),
      quiet = TRUE
    )
  
  #extract an stars object
  nc <- suppressWarnings(ereefs_extract(sf_obj, "2022-01-01", "2022-01-01", "Turbidity"))

  #check the function returns a stars object
  expect_s3_class(nc, "stars")
})
