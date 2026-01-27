test_that("Aggregation returns a smaller stars object (single attribute)", {

  #load in target data
  turb_reg <- stars::read_mdim(system.file("extdata/turb_reg.nc", package = "RcTools"))

  #create a vector of aggregation options and expected layer counts
  agg_options <- c("Month", "Season", "Financial", "Annual")
  expected_layers <- c(12, 3, 2, 1)

  #do the aggregations
  aggregated_data <- purrr::map(agg_options, \(x) nc_aggregation_helper(turb_reg, x))

  #check layer counts
  purrr::walk2(aggregated_data, expected_layers, \(x,y) expect_equal(dim(x)[[3]], y))

  #check attribute names
  purrr::walk(aggregated_data, \(x) expect_contains(names(x), "Turbidity (NTU)"))

  #check that aggregated values are not greater than input values
  purrr::walk(aggregated_data, \(x) {
    expect_lt(
      max(x$`Turbidity (NTU)`, na.rm = TRUE),
      max(turb_reg$`Turbidity (NTU)`, na.rm = TRUE)
    )
  })
})

test_that("Aggregation returns a smaller stars object (multi-attribute)", {

  #load in target data
  tc_reg <- stars::read_mdim(system.file("extdata/tc_reg.nc", package = "RcTools"))

  #create a vector of aggregation options, expected layer counts, and attribute names
  agg_options <- c("Month", "Season", "Financial", "Annual")
  expected_layers <- c(12, 3, 2, 1)
  att_names <- c("R_645", "R_555", "R_470")

  #do the aggregations
  aggregated_data <- purrr::map(agg_options, \(x) nc_aggregation_helper(tc_reg, x))

  #check layer counts
  purrr::walk2(aggregated_data, expected_layers, \(x,y) expect_equal(dim(x)[[3]], y))

  #check attribute names
  purrr::walk(aggregated_data, \(x) expect_contains(names(x), att_names))

  #check that aggregated values are not greater than input values
  for (att in att_names){
    purrr::walk(aggregated_data, \(x) {
      expect_lt(
        max(x[[att]], na.rm = TRUE),
        max(tc_reg[[att]], na.rm = TRUE)
      )
    })
  }
})