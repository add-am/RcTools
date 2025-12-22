#' Create a Windrose Plot from eReefs Data
#'
#' @param nc A list of NetCDF (stars) objects, generally produced by the [ereefs_extract()] function
#' @param SubSample Numeric String. The number of values per day to plot in the wind rose. Defaults to 500 values per day.
#' @param Heading Character String. The heading of the plot. Defaults to "Approximated Wind Speed".
#' @param LegendTitle Character String. The title of the legend. Defaults to "Speed (km/h)".

#' @returns A ggplot2 object (a windrose plot)
#'
#' @export
#' @examples
#' \dontrun{
#' wr_plot <- ereefs_windrose(nc)
#' }

ereefs_windrose <- function(nc, SubSample = 500, Heading = "Approximated Wind Speed", LegendTitle = "Speed (km/h)"){

  #check required argument
  if (missing(nc)){stop("You must supply at least the 'nc' parameter.")}

  #it is expected that the data is provided as a list of four netcdf files
  if (!inherits(nc, "list")){stop("Please supply a list of netCDF objects to the function.")}
  if (inherits(nc, "list")){
      if (any(!purrr::map_lgl(nc, \(nc_object) inherits(nc_object, "stars")))){
        stop("Each item in the list must be a netCDF (stars) object.")
      }
  }

  #continue to check argument types
  if (!is.numeric(SubSample)){stop("You must supply a numeric argument to the 'SubSample' parameter")}
  if (SubSample > 2000){warning("Using a SubSample value greater than 2000 will incur a significant processing cost and reduce function speed.")}

  #continue to check argument types
  if (!is.null(LegendTitle) & !is.character(LegendTitle)){stop("You must supply a character argument to the 'YAxisName' parameter")}
  if (!is.null(Heading) & !is.character(Heading)){stop("You must supply a character argument to the 'Heading' parameter")}

  #map over each object in the list and convert it to a dataframe  
  nc <- purrr::map(nc, \(x) {

    #convert the netcdf into a sf object (required by ggplot) and drop geometry
    full_data <- sf::st_drop_geometry(
      sf::st_as_sf(x, as_points = FALSE, merge = FALSE)
    )

    #take a subset of the data by iterating over each column in the sf object
    sub_sampled_data <- purrr::map_dfc(full_data, \(x) {sample(x, SubSample)})
    
    #edit the subset data
    sub_sampled_data <- sub_sampled_data |> 
      tidyr::pivot_longer(cols = tidyr::everything(), names_to = "Day", values_to = "Values") |> #pivot longer
      dplyr::mutate(
        Day = as.Date(Day), #format 
        Values = as.numeric(Values), #format
        Season = dplyr::case_when(lubridate::month(Day) > 4 & lubridate::month(Day) < 11 ~ "Dry", T ~ "Wet"), #calculate season
        Variable = names(x) #include the name of the variable so it can be identified later
      )
    
    return(sub_sampled_data)
  })

  #convert list of dataframes into a single object
  nc_bind <- do.call(rbind, nc)

  #further transform the data
  wind_data <- nc_bind |> 
    dplyr::mutate(
      Variable = dplyr::case_when( #update names to cleaner variations
        stringr::str_detect(Variable, "dir") ~ "wind_direction", 
        stringr::str_detect(Variable, "mag") ~ "wind_magnitude",
        stringr::str_detect(Variable, "wind_v") ~ "wind_v_component",
        T ~ "wind_u_component"
      )
    ) |>  
    dplyr::group_by(Day, Variable, Season) |> 
    dplyr::summarise(Values = mean(Values)) |> #obtain daily mean values
    dplyr::ungroup() |> 
    tidyr::pivot_wider(names_from = Variable, values_from = Values) |>  
    dplyr::mutate(wind_speed = sqrt(wind_magnitude/(1.225*1.3e-3))*3.6) #convert sheer stress (on the water) to approximate wind speed

  #create the windrose
  windrose <- climaemet::ggwindrose(
    speed = wind_data$wind_speed,
    direction = wind_data$wind_direction,
    n_directions = 8,
    n_speeds = 5,
    stack_reverse = TRUE,
    plot_title = Heading,
    legend_title = LegendTitle
  )

  #return the windrose
  return(windrose)

}

  
