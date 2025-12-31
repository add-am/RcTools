sf_obj <- system.file("extdata/boundary.gpkg", package = "RcTools")
sf_obj <- sf::st_read(sf_obj)
load_all()

#variations of extract
turb <- ereefs_extract(sf_obj, "2022-01-01", "2022-01-04", "Turbidity")
tc <- ereefs_extract(sf_obj, "2022-01-01", "2022-01-04", "True Colour")
wind <- ereefs_extract(sf_obj, "2022-01-01", "2022-01-04", "Wind")

#variations of reproject
turb <- ereefs_reproject(turb)
tc <- ereefs_reproject(tc)
wind <- ereefs_reproject(wind)

#dotplot
turb_dp <- ereefs_dotplot(turb)
#tc_dp <- ereefs_dotplot(tc) #fails (that fine)
#wind_dp <- ereefs_dotplot(wind) #fails (also fine)

turb_dp

#windrose
wind_r <- ereefs_windrose(wind)

wind_r

#aggregation helper
turb_agg <- ereefs_aggregation_helper(turb, "Month")
tc_agg <- ereefs_aggregation_helper(tc, "Month")
wind_agg <- ereefs_aggregation_helper(wind, "Month")

#map
turb_map <- ereefs_map(turb, "Concentration", Aggregation = "Month")
tc_map <- ereefs_map(tc, "True Colour", Aggregation = "Month")
wind_map <- ereefs_map(wind, "Vector Field", Aggregation = "Month")

turb_map
tc_map
wind_map
