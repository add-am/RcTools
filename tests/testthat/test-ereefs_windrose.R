test_that("ereefs_windrose returns a ggplot object for multi-attribute wind stars datasets", {
  
  #load in target data
  nc <- stars::read_mdim(system.file("extdata/wind_reg.nc", package = "RcTools"))

  #create a list from the data
  nc_list <- list(nc[,,,1:100], nc[,,,101:200], nc[,,,201:365])

  #run the function under a variety of conditions
  wr_1 <- suppressWarnings(ereefs_windrose(nc)) #generally
  wr_2 <- suppressWarnings(ereefs_windrose(nc_list)) #accepting a list

  #provide all options to the aggregation argument
  agg_ops <- c("Month", "Season", "Financial", "Annual")

  wr_3 <- purrr::map(agg_ops, \(x) {
    suppressWarnings(
      ereefs_windrose(nc, #specifying all arguments
        SubSample = 500, 
        Aggregation = x, 
        Heading = "Generic Heading", 
        LegendTitle = "Generic Legend"
      )
    )
  })

  #check the type of the output (about the only thing that can be done)
  expect_s3_class(wr_1, "ggplot2::ggplot")
  expect_s3_class(wr_2, "ggplot2::ggplot")
  purrr::walk(wr_3, \(x) expect_s3_class(x, "ggplot2::ggplot"))
  
})
