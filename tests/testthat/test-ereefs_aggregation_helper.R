test_that("Aggregation returns a smaller stars object", {

  #load in data
  multi_att_reg <- stars::read_mdim(system.file("extdata/multi_att_reg.nc", package = "RcTools"))

  #aggregate the object across all options
  muti_att_agg_month <- ereefs_aggregation_helper(multi_att_reg, "Month")
  muti_att_agg_season <- ereefs_aggregation_helper(multi_att_reg, "Season")
  muti_att_agg_financial <- ereefs_aggregation_helper(multi_att_reg, "Financial")
  muti_att_agg_annual <- ereefs_aggregation_helper(multi_att_reg, "Annual")
}
)