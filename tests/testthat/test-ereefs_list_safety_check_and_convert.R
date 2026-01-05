test_that("ereefs_list_safety_check_and_convert returns a single attribute stars object", {

  #load in target dataset
  turb_reg <- stars::read_mdim(system.file("extdata/turb_reg.nc", package = "RcTools"))

  #create a list from the dataset
  list_of_stars <- list(turb_reg[,,,1], turb_reg[,,,2], turb_reg[,,,3])

  #run the safety check
  out_converted <- ereefs_list_safety_check_and_convert(list_of_stars)
  out_same <- ereefs_list_safety_check_and_convert(turb_reg)

  #check that:
  expect_s3_class(out_converted, "stars") #a single stars object is returned (if list provided)
  expect_equal(dim(out_converted)[[3]], 3) #the object has a time dimension of equal length to the overall list time length
  expect_identical(out_same, turb_reg) #or the original object is returned (if stars provided)
  
})

test_that("ereefs_list_safety_check_and_convert returns a multi attribute stars object", {

  #load in target dataset
  tc_reg <- stars::read_mdim(system.file("extdata/tc_reg.nc", package = "RcTools"))

  #create a list from the dataset
  list_of_stars <- list(tc_reg[,,,1], tc_reg[,,,2], tc_reg[,,,3])

  #run the safety check
  out_converted <- ereefs_list_safety_check_and_convert(list_of_stars)
  out_same <- ereefs_list_safety_check_and_convert(tc_reg)

  #check that:
  expect_s3_class(out_converted, "stars") #a single stars object is returned (if list provided)
  expect_equal(dim(out_converted)[[3]], 3) #the object has a time dimension of equal length to the overall list time length
  expect_identical(out_same, tc_reg) #or the original object is returned (if stars provided)
  
})
