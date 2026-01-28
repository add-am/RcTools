test_that("ereefs_extract returns a single attribute stars object", {

  skip("Slow test — periodically unskip and run manually")

  #load in a small boundary
  sf_obj <- sf::st_read(
      system.file("extdata/boundary.gpkg", package = "RcTools"),
      quiet = TRUE
    )
  
  #build a matrix of variations to try (essentially a single time step and a multi timestep option)
  variations <- data.frame(
    Variable = c("Turbidity", "Turbidity"),
    StartDate = c("2022-01-01", "2022-01-01"),
    EndDate = c("2022-01-01", "2022-01-03")
  )
  
  #extract each stars object
  nc_list <- suppressMessages(
    suppressWarnings(
      purrr::pmap(variations, \(Variable, StartDate, EndDate)
        ereefs_extract(sf_obj, StartDate, EndDate, Variable)
      )
    )
  )

  #check each object
  purrr::walk2(nc_list, c(1, 3), \(x,y) {
    expect_s3_class(x, "stars") #it returns a stars object
    expect_equal(length(dim(x)), 3) #it has 3 dimensions
    expect_equal(dim(x)[[3]], y) #it has n time layers (equal to the number of time layers requested)
    expect_contains(names(x), "Turbidity (NTU)") #the attribute name contains the right variable and unit
    expect_contains(sf::st_crs(x)[[1]], "OGC:CRS84") #the crs is the global WGS84
  })
})

test_that("ereefs_extract returns a multi-attribute TC stars object", {

  skip("Slow test — periodically unskip and run manually")

  #load in a small boundary
  sf_obj <- sf::st_read(
      system.file("extdata/boundary.gpkg", package = "RcTools"),
      quiet = TRUE
    )
  
  #build a matrix of variations to try (essentially a single time step and a multi timestep option)
  variations <- data.frame(
    Variable = c("True Colour", "True Colour"),
    StartDate = c("2022-01-01", "2022-01-01"),
    EndDate = c("2022-01-01", "2022-01-03")
  )
  
  #extract each stars object
  nc_list <- suppressMessages(
    suppressWarnings(
      purrr::pmap(variations, \(Variable, StartDate, EndDate)
        ereefs_extract(sf_obj, StartDate, EndDate, Variable)
      )
    )
  )

  #check each object
  purrr::walk2(nc_list, c(1,3), \(x,y) {
    expect_s3_class(x, "stars") #it returns a stars object
    expect_equal(length(dim(x)), 3) #it has 3 dimensions
    expect_equal(dim(x)[[3]], y) #it has n time layers (equal to the number of time layers requested)
    expect_contains(names(x), c("R_645", "R_555", "R_470")) #the attribute name contains the right variable and unit
    expect_contains(sf::st_crs(x)[[1]], "OGC:CRS84") #the crs is the global WGS84
  })
})

test_that("ereefs_extract returns a multi-attribute Wind stars object", {

  skip("Slow test — periodically unskip and run manually")

  #load in a small boundary
  sf_obj <- sf::st_read(
      system.file("extdata/boundary.gpkg", package = "RcTools"),
      quiet = TRUE
    )
  
  #build a matrix of variations to try (essentially a single time step and a multi timestep option)
  variations <- data.frame(
    Variable = c("Wind", "Wind"),
    StartDate = c("2022-01-01", "2022-01-01"),
    EndDate = c("2022-01-01", "2022-01-03")
  )
  
  #extract each stars object
  nc_list <- suppressMessages(
    suppressWarnings(
      purrr::pmap(variations, \(Variable, StartDate, EndDate)
        ereefs_extract(sf_obj, StartDate, EndDate, Variable)
      )
    )
  )

  #check each object
  purrr::walk2(nc_list, c(1,3), \(x,y) {
    expect_s3_class(x, "stars") #it returns a stars object
    expect_equal(length(dim(x)), 3) #it has 3 dimensions
    expect_equal(dim(x)[[3]], y) #it has n time layers (equal to the number of time layers requested)
    expect_contains(names(x), c("wind_dir (degrees)", "wind_mag (Nm-2)", "wind_u (Nm-2)", "wind_v (Nm-2)")) #the attribute name contains the right variable and unit
    expect_contains(sf::st_crs(x)[[1]], "OGC:CRS84") #the crs is the global WGS84
  })
})
