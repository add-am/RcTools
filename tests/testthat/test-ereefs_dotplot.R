test_that("ereefs_dotplot returns a ggplot object for single attribute stars datasets", {
  
  #load in target data
  nc <- stars::read_mdim(system.file("extdata/turb_reg.nc", package = "RcTools"))

  #create a list from the data
  nc_list <- list(nc[,,,1:100], nc[,,,101:200], nc[,,,201:365])

  #run the function under a variety of conditions
  dp_1 <- ereefs_dotplot(nc) #generally
  dp_2 <- ereefs_dotplot(nc_list) #accepting a list
  dp_3 <- ereefs_dotplot(nc, #specifying all arguments
    SubSample = 500, Heading = "X", 
    YAxisName = "Y", LogTransform = TRUE, 
    AttributeName = "Turbidity (NTU)"
  )

  #check the type of the output (about the only thing that can be done)
  expect_s3_class(dp_1, "ggplot2::ggplot")
  expect_s3_class(dp_2, "ggplot2::ggplot")
  expect_s3_class(dp_3, "ggplot2::ggplot")
  
})

test_that("ereefs_dotplot returns a ggplot object for multi-attribute TC stars datasets", {
  
  #load in target data
  nc <- stars::read_mdim(system.file("extdata/tc_reg.nc", package = "RcTools"))

  #create a list from the data
  nc_list <- list(nc[,,,1:100], nc[,,,101:200], nc[,,,201:365])

  #run the function under a variety of conditions
  dp_1 <- ereefs_dotplot(nc, AttributeName = "R_645") #generally
  dp_2 <- ereefs_dotplot(nc_list, AttributeName = "R_645") #accepting a list
  dp_3 <- ereefs_dotplot(nc, #specifying all arguments
    SubSample = 500, Heading = "X", 
    YAxisName = "Y", LogTransform = TRUE, 
    AttributeName = "R_645"
  )

  #check the type of the output (about the only thing that can be done)
  expect_s3_class(dp_1, "ggplot2::ggplot")
  expect_s3_class(dp_2, "ggplot2::ggplot")
  expect_s3_class(dp_3, "ggplot2::ggplot")
  
})

test_that("ereefs_dotplot returns a ggplot object for multi-attribute wind stars datasets", {
  
  #load in target data
  nc <- stars::read_mdim(system.file("extdata/wind_reg.nc", package = "RcTools"))

  #create a list from the data
  nc_list <- list(nc[,,,1:100], nc[,,,101:200], nc[,,,201:365])

  #run the function under a variety of conditions
  dp_1 <- ereefs_dotplot(nc, AttributeName =  "wind_mag (Nm-2)") #generally
  dp_2 <- ereefs_dotplot(nc_list, AttributeName =  "wind_mag (Nm-2)") #accepting a list
  dp_3 <- ereefs_dotplot(nc, #specifying all arguments
    SubSample = 500, Heading = "X", 
    YAxisName = "Y", LogTransform = TRUE, 
    AttributeName =  "wind_mag (Nm-2)"
  )

  #check the type of the output (about the only thing that can be done)
  expect_s3_class(dp_1, "ggplot2::ggplot")
  expect_s3_class(dp_2, "ggplot2::ggplot")
  expect_s3_class(dp_3, "ggplot2::ggplot")
  
})
