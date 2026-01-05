test_that("ereefs_map returns a tmap object for single attribute stars datasets", {
  
  #load in target data
  nc <- stars::read_mdim(system.file("extdata/turb_reg.nc", package = "RcTools"))

  #create a list from the data
  nc_list <- list(nc[,,,1:100], nc[,,,101:200], nc[,,,201:365])

  #run the function under a variety of conditions
  m_1 <- ereefs_map(nc, MapType = "Concentration") #generally
  m_2 <- ereefs_map(nc_list, MapType = "Concentration") #accepting a list

  #provide all options to the aggregation argument
  agg_ops <- c("Month", "Season", "Financial", "Annual")

  m_3 <- purrr::map(agg_ops, \(x) {
    ereefs_map(nc, 
      MapType = "Concentration", 
      Aggregation = x, 
      LegendTitle = "Generic Text", 
      nrow = 2
    )
  })

  #check the type of the output (about the only thing that can be done)
  expect_s3_class(m_1, "tmap")
  expect_s3_class(m_2, "tmap")
  purrr::walk(m_3, \(x) expect_s3_class(x, "tmap"))
  
})

test_that("ereefs_map returns a tmap object for multi-attribute true colour stars datasets", {
  
  #load in target data
  nc <- stars::read_mdim(system.file("extdata/tc_reg.nc", package = "RcTools"))

  #create a list from the data
  nc_list <- list(nc[,,,1:100], nc[,,,101:200], nc[,,,201:365])

  #run the function under a variety of conditions
  m_1 <- ereefs_map(nc, MapType = "True Colour") #generally
  m_2 <- ereefs_map(nc_list, MapType = "True Colour") #accepting a list

  #provide all options to the aggregation argument
  agg_ops <- c("Month", "Season", "Financial", "Annual")

  m_3 <- purrr::map(agg_ops, \(x) {
    ereefs_map(nc, 
      MapType = "True Colour", 
      Aggregation = x, 
      LegendTitle = "Generic Text", 
      nrow = 2
    )
  })

  #check the type of the output (about the only thing that can be done)
  expect_s3_class(m_1, "tmap")
  expect_s3_class(m_2, "tmap")
  purrr::walk(m_3, \(x) expect_s3_class(x, "tmap"))
  
})

test_that("ereefs_map returns a tmap object for multi-attribute wind stars datasets", {
  
  #load in target data
  nc <- stars::read_mdim(system.file("extdata/wind_reg.nc", package = "RcTools"))

  #create a list from the data
  nc_list <- list(nc[,,,1:100], nc[,,,101:200], nc[,,,201:365])

  #run the function under a variety of conditions
  m_1 <- ereefs_map(nc, MapType = "Vector Field") #generally
  m_2 <- ereefs_map(nc_list, MapType = "Vector Field") #accepting a list

  #provide all options to the aggregation argument
  agg_ops <- c("Month", "Season", "Financial", "Annual")

  m_3 <- purrr::map(agg_ops, \(x) {
    ereefs_map(nc, 
      MapType = "Vector Field", 
      Aggregation = x, 
      LegendTitle = "Generic Text", 
      nrow = 2
    )
  })

  #check the type of the output (about the only thing that can be done)
  expect_s3_class(m_1, "ggplot2::ggplot")
  expect_s3_class(m_2, "ggplot2::ggplot")
  purrr::walk(m_3, \(x) expect_s3_class(x, "ggplot2::ggplot"))
  
})