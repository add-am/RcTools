#' A function to help with various forms of netCDF aggregation
#'
#' @param nc A netCDF object
#' @param agg The type of aggregation to complete, defaults to "Month". Options are "Month", "Season", "Financial", "Annual"
#' 
#' @returns A netCDF object
#'
#' @export
#' 
#' @examples
#' 
#' nc <- system.file("extdata/turb_reg.nc", package = "RcTools")
#' nc <- stars::read_mdim(nc)
#' 
#' x <- nc_aggregation_helper(nc, "Month")
#'  
nc_aggregation_helper <- function(nc, agg){

  #pull out the attribute name/names
  attribute_names <- names(nc)

  #extract dates into a dataframe
  nc_dates <- data.frame(DateTime = stars::st_get_dimension_values(nc, "time"))

  #expand the dates dataframe to have several variations of the dates
  nc_dates <- nc_dates |> 
    dplyr::mutate(
      Year = lubridate::year(DateTime),
      Fyear = busdater::get_fy(DateTime),
      Month = lubridate::month(DateTime),
      Season = dplyr::case_when(Month > 4 & Month < 11 ~ "Dry", T ~ "Wet"),
      SeasonYear = dplyr::case_when(
        Season == "Dry" ~ Year, 
        Season == "Wet" & Month >= 11 ~ Year, 
        Season == "Wet" & Month <= 4 ~ Year - 1
      ),
      RowId = dplyr::row_number()
    )
    
  #depending on what aggregation is requested, change how the layer name column is created
  if (agg == "Month") {nc_dates <- tidyr::unite(nc_dates, LayerName, "Year", "Month", sep = "_")
  } else if (agg == "Season") {nc_dates <- tidyr::unite(nc_dates, LayerName, "SeasonYear", "Season", sep = "_")
  } else if (agg == "Financial") {nc_dates <- dplyr::mutate(nc_dates, LayerName = Fyear)
  } else if (agg == "Annual") {nc_dates <- dplyr::mutate(nc_dates, LayerName = Year)}
    
  #based on the aggregation, determine the associated row (and thus layer) numbers to select
  dates <- nc_dates  |> 
    dplyr::group_by(LayerName) |> 
    dplyr::summarise(
      MinIndex = min(RowId),
      MaxIndex = max(RowId)
    ) |> 
    dplyr::arrange(MinIndex)

  #map over these layer numbers
  sliced_nc <- purrr::pmap(dates, \(LayerName, MinIndex, MaxIndex){

    #slice the data
    sliced_layers <- nc[,,,MinIndex:MaxIndex]

    #apply the mean function to the slice
    aggregated_layer <- stars::st_apply(sliced_layers, 1:2, FUN = mean, keep = TRUE)

    #manually add the time dimension back in
    aggregated_layer <- stars::st_redimension(
      aggregated_layer,
      new_dims = c(dim(aggregated_layer), 1),
      along = #stats::setNames( (dont need this stuff i think?)
        list(LayerName)#, "time"), (dont need this stuff i think?)
      #name = "time" (dont need this stuff i think?)
    )

    #it requires this two step process first to build the dimension then to properly add the value
    stars::st_dimensions(aggregated_layer)$X1$values <- LayerName   
      
    #expand the layer name re naming the attributes afterwards
    #LayerName <- attribute_names

    #give the new layer a name
    names(aggregated_layer) <- attribute_names

    return(aggregated_layer)

    })
    
  #bind the data back together
  aggregated_item <- do.call(c, c(sliced_nc, along = "X1"))

  #fix the dimension names
  aggregated_item <- stars::st_set_dimensions(aggregated_item, names = c("x", "y", "time"))

  #update the names of the attributes
  names(aggregated_item) <- attribute_names

  #return the final product
  return(aggregated_item)

}
