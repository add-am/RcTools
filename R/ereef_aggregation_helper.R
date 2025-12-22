#' A function to help with various forms of netCDF aggregation
#'
#' @param nc A netCDF object
#' @param agg The type of aggregation to complete, defaults to monthly
#' 
#' @returns A netCDF object
#'
#' @examples
ereefs_aggregation_helper <- function(nc, agg){

  #extract dates
  nc_dates <- data.frame(DateTime = stars::st_get_dimension_values(nc, "time"))

  #expand the dates dataframe
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
  if (agg == "Monthly") {nc_dates <- tidyr::unite(nc_dates, LayerName, "Year", "Month", sep = "_")
  } else if (agg == "Seasonal") {nc_dates <- tidyr::unite(nc_dates, LayerName, "SeasonYear", "Season", sep = "_")
  } else if (agg == "Financial") {nc_dates <- tidyr::mutate(nc_dates, LayerName = Year)
  } else if (agg == "Annual") {nc_dates <- tidyr::mutate(nc_dates, LayerName = Fyear)}
  
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

    #slice the data THIS WILL DEPEND ON THE NUMBER OF DIMENSIONS AND ATTRIBUTES. MORE FOR WIND AND RGB!
    sliced_layers <- nc[,,,MinIndex:MaxIndex]

    #apply the mean function to the slice
    aggregated_layer <- stars::st_apply(sliced_layers, 1:2, FUN = mean, keep = TRUE)

    #give the new layer a name
    names(aggregated_layer) <- LayerName

    return(aggregated_layer)

  })

  #bind the data back together
  aggregated_nc <- do.call(c, sliced_nc)

  #and fix the time values
  aggregated_nc <- stars::st_redimension(
    aggregated_nc,
    new_dims = stars::st_dimensions(aggregated_nc),
    along = setNames(list(names(aggregated_nc)), "time")
  )







}