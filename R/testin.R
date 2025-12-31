sf_obj <- system.file("extdata/boundary.gpkg", package = "RcTools")
sf_obj <- sf::st_read(sf_obj)
load_all()

#variations of extract
turb_1 <- ereefs_extract(sf_obj, "2022-01-01", "2022-01-04", "Turbidity") #testing concentration generally
turb_2 <- ereefs_extract(sf_obj, "2022-01-05", "2022-01-05", "Turbidity") #testing if time is preserved on one step
turb_3 <- ereefs_extract(sf_obj, "2022-01-14", "2022-01-16", "Turbidity") #testing the ability to combine multi-attribute files

tc_1 <- ereefs_extract(sf_obj, "2022-01-01", "2022-01-04", "True Colour") #testing tc generally
tc_2 <- ereefs_extract(sf_obj, "2022-01-05", "2022-01-05", "True Colour") #testing if time is preserved on one step
tc_3 <- ereefs_extract(sf_obj, "2022-01-14", "2022-01-16", "True Colour") #testing the ability to combine multi-attribute files

wind_1 <- ereefs_extract(sf_obj, "2022-01-01", "2022-01-04", "Wind") #testing wind generally
wind_2 <- ereefs_extract(sf_obj, "2022-01-05", "2022-01-05", "Wind") #testing if time is preserved on one step
wind_3 <- ereefs_extract(sf_obj, "2022-01-14", "2022-01-16", "Wind") #testing the ability to combine multi-attribute files

#variations of reproject on each of the files above (all work)
turb_1 <- ereefs_reproject(turb_1)
turb_2 <- ereefs_reproject(turb_2)
turb_3 <- ereefs_reproject(turb_3)

tc_1 <- ereefs_reproject(tc_1)
tc_2 <- ereefs_reproject(tc_2)
tc_3 <- ereefs_reproject(tc_3)

wind_1 <- ereefs_reproject(wind_1)
wind_2 <- ereefs_reproject(wind_2)
wind_3 <- ereefs_reproject(wind_3)

#dotplot
turb_dp <- ereefs_dotplot(turb_1) #testing general usage for a single attribute dataset
tc_dp <- ereefs_dotplot(tc_1, AttributeName = "R_645") #testing usage for a multi-attribute dataset
wind_dp <- ereefs_dotplot(wind_1, AttributeName = "wind_mag (Nm-2)") #testing usage for a multi-attribute dataset
turb_dp_list <- ereefs_dotplot(list(turb_1, turb_2, turb_3)) #testing acceptance of list, and catch failures
turb_dp_list <- ereefs_dotplot(list(turb_1, turb_3)) #testing acceptance of list, and catch failures
tc_dp_list <- ereefs_dotplot(list(tc_1, tc_2, tc_3), AttributeName = "R_645") #testing acceptance of list
wind_dp_list <- ereefs_dotplot(list(wind_1, wind_2, wind_3), AttributeName = "wind_mag (Nm-2)") #testing acceptance of list

turb_dp
tc_dp
wind_dp
turb_dp_list
tc_dp_list
wind_dp_list

#windrose
wind_r <- ereefs_windrose(wind_1) #testing general usage
wind_r_list <- ereefs_windrose(list(wind_1, wind_2, wind_3)) #testing acceptance of a list

wind_r
wind_r_list

#aggregation helper
turb_1_agg <- ereefs_aggregation_helper(turb_1, "Month")
turb_2_agg <- ereefs_aggregation_helper(turb_2, "Month")
turb_3_agg <- ereefs_aggregation_helper(turb_3, "Month")
tc_1_agg <- ereefs_aggregation_helper(tc_1, "Month")
tc_2_agg <- ereefs_aggregation_helper(tc_2, "Month")
tc_3_agg <- ereefs_aggregation_helper(tc_3, "Month")
wind_1_agg <- ereefs_aggregation_helper(wind_1, "Month")
wind_2_agg <- ereefs_aggregation_helper(wind_2, "Month")
wind_3_agg <- ereefs_aggregation_helper(wind_3, "Month")

turb_1_agg
turb_2_agg
turb_3_agg
tc_1_agg
tc_2_agg
tc_3_agg
wind_1_agg
wind_2_agg
wind_3_agg

#map
turb_1_map <- ereefs_map(turb_1, "Concentration", Aggregation = "Month")
turb_2_map <- ereefs_map(turb_2, "Concentration", Aggregation = "Month")
turb_3_map <- ereefs_map(turb_3, "Concentration", Aggregation = "Month")
turb_list_map <- ereefs_map(list(turb_1, turb_2, turb_3), "Concentration", Aggregation = "Month")
turb_list_map <- ereefs_map(list(turb_1, turb_3), "Concentration", Aggregation = "Month")
tc_1_map <- ereefs_map(tc_1, "True Colour", Aggregation = "Month")
tc_2_map <- ereefs_map(tc_2, "True Colour", Aggregation = "Month")
tc_3_map <- ereefs_map(tc_3, "True Colour", Aggregation = "Month")
tc_list_map <- ereefs_map(list(tc_1, tc_2, tc_3), "True Colour", Aggregation = "Month")
wind_1_map <- ereefs_map(wind_1, "Vector Field", Aggregation = "Month")
wind_2_map <- ereefs_map(wind_2, "Vector Field", Aggregation = "Month")
wind_3_map <- ereefs_map(wind_3, "Vector Field", Aggregation = "Month")
wind_list_map <- ereefs_map(list(wind_1, wind_2, wind_3), "Vector Field", Aggregation = "Month")

turb_1_map
turb_2_map
turb_3_map
turb_list_map
tc_1_map
tc_2_map
tc_3_map
tc_list_map
wind_1_map
wind_2_map
wind_3_map
wind_list_map

#things to check still
# longer time frames, e.g. several months
# other aggregation options e.g., seasonal, fy

