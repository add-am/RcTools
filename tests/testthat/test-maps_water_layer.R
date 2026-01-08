test_that("maps_water_layer returns a tmap object", {

  #run the function in a variety of ways
  black <- maps_water_layer(Basin = "Black")
  ross_and_black <- maps_water_layer(Basin = c("Black", "Ross"))
  all_types <- maps_water_layer("Black", WaterLines = TRUE, WaterAreas = TRUE, WaterLakes = TRUE)
  stream_order_single <- maps_water_layer(Basin = "Black", StreamOrder = 3)
  stream_order_double <- maps_water_layer(Basin = "Black", StreamOrder = c(3, 5))

  #confirm that each of them return a tmap object
  expect_s3_class(black, "tmap")
  expect_s3_class(ross_and_black, "tmap")
  expect_s3_class(all_types, "tmap")
  expect_s3_class(stream_order_single, "tmap")
  expect_s3_class(stream_order_double, "tmap")
  
})
