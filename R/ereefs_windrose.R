#' Create a Windrose Plot from eReefs Data
#'
#' @param nc A single NetCDF (stars) object OR a list of NetCDF (stars) objects, generally produced by the [ereefs_extract()] function
#' @param SubSample Numeric String. The number of values per day to plot in the wind rose. Defaults to 500 values per day.
#' @param Aggregation Character String. What type of grouping to apply to the data. Defaults to "Month". Options are "Month", "Season", "Financial", "Annual"
#' @param Heading Character String. The heading of the plot. Defaults to "Approximated Wind Speed".
#' @param LegendTitle Character String. The title of the legend. Defaults to "Speed (km/h)".

#' @returns A ggplot2 object (a windrose plot)
#'
#' @export
#' @examples
#' 
#' nc <- system.file("extdata/wind_reg.nc", package = "RcTools")
#' nc <- stars::read_mdim(nc)
#' 
#' wr_plot <- ereefs_windrose(nc)
#' 
ereefs_windrose <- function(nc, SubSample = 500, Aggregation = "Month", Heading = "Approximated Wind Speed", LegendTitle = "Speed (km/h)"){

  #check required argument
  if (missing(nc)){stop("You must supply at least the 'nc' parameter.")}

  #check if single object or list, and convert
  nc <- ereefs_list_safety_check_and_convert(nc)
  
  #continue to check argument types
  if (!is.numeric(SubSample)){stop("You must supply a numeric argument to the 'SubSample' parameter")}
  if (SubSample > 2000){warning("Using a SubSample value greater than 2000 will incur a significant processing cost and reduce function speed.")}
  if (!is.character(Aggregation)){stop("You must supply a character argument to the 'Aggregation' parameter.")}
  if (!is.null(LegendTitle) & !is.character(LegendTitle)){stop("You must supply a character argument to the 'YAxisName' parameter")}
  if (!is.null(Heading) & !is.character(Heading)){stop("You must supply a character argument to the 'Heading' parameter")}

  #convert the netcdf into a sf object (required by ggplot) and drop geometry
  full_data <- sf::st_drop_geometry(
    sf::st_as_sf(nc, as_points = FALSE, merge = FALSE)
  )

  #take a subset of the data by iterating over each column in the sf object
  sub_sampled_data <- purrr::map_dfc(full_data, \(x) {sample(x, SubSample)})

  #mutate and edit data
  wind_data <- sub_sampled_data |> 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |> #formatting
    tidyr::pivot_longer(
      cols = dplyr::everything(), 
      names_to = c("Variable", "DateTime"), 
      names_sep = "\\.", 
      values_to = "Values"
    ) |> 
    dplyr::mutate(
      Values = as.numeric(Values),
      DateTime = as.Date(DateTime),
      Year = lubridate::year(DateTime),
      Fyear = busdater::get_fy(DateTime),
      Month = lubridate::month(DateTime),
      Season = dplyr::case_when(Month > 4 & Month < 11 ~ "Dry", T ~ "Wet"), 
      SeasonYear = dplyr::case_when(
        Season == "Dry" ~ Year,
        Season == "Wet" & Month >= 11 ~ Year,
        Season == "Wet" &  Month <= 4 ~ Year - 1
      ),
      Variable = dplyr::case_when( #update names to cleaner variations
        stringr::str_detect(Variable, "dir") ~ "wind_direction", 
        stringr::str_detect(Variable, "mag") ~ "wind_magnitude",
        stringr::str_detect(Variable, "wind_v") ~ "wind_v_component",
        T ~ "wind_u_component"
      )
    ) |>
    dplyr::group_by(DateTime, Variable) |> 
    dplyr::mutate(RowId = dplyr::row_number()) |> 
    dplyr::ungroup() |> 
    tidyr::pivot_wider(names_from = Variable, values_from = Values) |> 
    dplyr::mutate(wind_speed = sqrt(wind_magnitude/(1.225*1.3e-3))*3.6) #convert sheer stress (on the water) to approximate wind speed
    
    #depending on what aggregation is requested, change how the dataframe grouping column is built
    if (Aggregation == "Month") {wind_data <- tidyr::unite(wind_data, GroupingCol, "Year", "Month", sep = "_")}
    else if (Aggregation == "Season") {wind_data <- tidyr::unite(wind_data, GroupingCol, "SeasonYear", "Season", sep = "_")}
    else if (Aggregation == "Financial") {wind_data <- dplyr::mutate(wind_data, GroupingCol = as.character(Year))}
    else if (Aggregation == "Annual") {wind_data <- dplyr::mutate(wind_data, GroupingCol = as.character(Fyear))}

  #create the windrose, use the grouping column to define facets
  windrose <- 
    climaemet::ggwindrose(
      speed = wind_data$wind_speed,
      direction = wind_data$wind_direction,
      n_directions = 8,
      n_speeds = 5,
      stack_reverse = TRUE,
      plot_title = Heading,
      legend_title = LegendTitle,
      facet = wind_data$GroupingCol
    )
  
  #return the windrose
  return(windrose)

}

  
