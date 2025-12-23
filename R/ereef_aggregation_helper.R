#' A function to help with various forms of netCDF aggregation
#'
#' @param nc A netCDF object
#' @param agg The type of aggregation to complete, defaults to monthly. Options are "Month", "Season", "Financial", "Annual"
#' 
#' @returns A netCDF object
#'
#' @examples
ereefs_aggregation_helper <- function(nc, agg){

  #data can be provided as a list of nc objects (wind) or a single object. If a single object, we just need to wrap it
  #in a list so it can pass through the next purrr map
  if (!inherits(nc, "list")){nc <- list(nc)}

  #map over each element in the list of objects, in most instances this is only one (but four for wind)
  aggregated_data <- purrr::map(nc, \(x){

    #pull the name(s) out of the object before it undergoes aggregation
    attribute_names <- names(x)

    #extract dates into a dataframe
    nc_dates <- data.frame(DateTime = stars::st_get_dimension_values(x, "time"))

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

      #slice the data
      sliced_layers <- x[,,,MinIndex:MaxIndex]

      #apply the mean function to the slice
      aggregated_layer <- stars::st_apply(sliced_layers, 1:2, FUN = mean, keep = TRUE)

      #if there is more than one attribute dimension, make addition layer names and add the dimension manually
      if (length(names(aggregated_layer)) > 1){

        #manually add the time dimension back in
        aggregated_layer <- stars::st_redimension(
          aggregated_layer,
          new_dims = c(295, 276, 1),
          along = setNames(list(LayerName), "time"),
          name = "time"
        )

        #it requires this two step process first to build the dimension then to properly add the value
        stars::st_dimensions(aggregated_layer)$X3$values <- LayerName   
        
        #expand the layer name re naming the attributes afterwards
        LayerName <- attribute_names
      }

      #give the new layer a name
      names(aggregated_layer) <- LayerName

      return(aggregated_layer)

      })
    
    if (length(names(sliced_nc[[1]])) > 1){
    
      #bind the data back together
      aggregated_item <- do.call(c, c(sliced_nc, along = "X3"))

      #fix the dimension names
      aggregated_item <- stars::st_set_dimensions(aggregated_item, names = c("x", "y", "time"))

    } else {

      #bind the data back together
      aggregated_item <- do.call(c, sliced_nc)

      #and fix the time values
      aggregated_item <- stars::st_redimension(
        aggregated_item,
        new_dims = stars::st_dimensions(aggregated_item),
        along = setNames(list(names(aggregated_item)), "time")
      )

      #replace the name of the aggregated attribute
      names(x) <- attribute_names
    }

    #return the aggregated item
    return(aggregated_item)

  })

  #if the list output has length one, it can be converted back to a individual stars object
  if (length(aggregated_data) == 1){

    aggregated_data <- aggregated_data[[1]]

  #if the list output has several items it is wind, and the names need to be extracted
  } else {

    aggregated_data <- purrr::map2(aggregated_data, nc, \(x, y) {

      names(x) <- names(y)

      return(x)
    })
  }

  #return the final product
  return(aggregated_data)

}