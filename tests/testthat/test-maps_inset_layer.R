test_that("maps_inset_layer returns a tmap object", {

  #load in required sf objects
  sf_obj_1 <- sf::st_read(
    system.file("extdata/ross.gpkg", package = "RcTools"),
    quiet = TRUE
  )
  
  sf_obj_2 <- sf::st_read(
    system.file("extdata/dry_tropics.gpkg", package = "RcTools"),
     quiet = TRUE
  )
  
  sf_obj_3 <- sf::st_read(
    system.file("extdata/black.gpkg", package = "RcTools"),
    quiet = TRUE
  )
  
  #run the function in a variety of ways
  inset_1 <- maps_inset_layer(supplied_sf_1 = sf_obj_1, background = sf_obj_2, aspect = 1.1)

  inset_2 <- maps_inset_layer(
    supplied_sf_1 = sf_obj_1, 
    background = sf_obj_2, 
    aspect = 1.1, 
    use_bbox_1 = FALSE, 
    supplied_sf_1_colour = "orange", 
    background_colour = "pink"
  )

  inset_3 <- maps_inset_layer(
    supplied_sf_1 = sf_obj_1, 
    background = sf_obj_2, 
    aspect = 1.1, 
    use_bbox_1 = FALSE, 
    supplied_sf_1_colour = "orange", 
    background_colour = "pink", 
    supplied_sf_2 = sf_obj_3, 
    use_bbox_2 = TRUE, 
    supplied_sf_2_colour = "green"
  )

  inset_4 <- maps_inset_layer(
    supplied_sf_1 = sf_obj_1, 
    background = sf_obj_2, 
    aspect = 1.1, 
    use_bbox_1 = FALSE, 
    supplied_sf_1_colour = "orange", 
    background_colour = "pink", 
    supplied_sf_2 = sf_obj_3, 
    use_bbox_2 = FALSE, 
    supplied_sf_2_colour = "green"
  )

  inset_5 <- maps_inset_layer(
    supplied_sf_1 = sf_obj_1, 
    background = sf_obj_2, 
    aspect = 1.1, 
    use_bbox_1 = TRUE, 
    supplied_sf_1_colour = "orange", 
    background_colour = "pink", 
    supplied_sf_2 = sf_obj_3, 
    use_bbox_2 = TRUE, 
    supplied_sf_2_colour = "green"
  )

  #join all objects into a list
  all_objects <- list(inset_1, inset_2, inset_3, inset_4, inset_5)

  #confirm that each list contains 1 tmap object and one viewport object
  purrr::walk(all_objects, \(x) {
    expect_s3_class(x[[1]], "tmap")
    expect_s3_class(x[[2]], "viewport")
  })
})
