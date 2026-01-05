#' Plotting eReefs Data
#'
#' @param nc A single NetCDF (stars) object OR a list of NetCDF (stars) objects, generally produced by the [ereefs_extract()] function
#' @param SubSample Numeric String. The number of values per day to plot in the dot plot. Defaults to 500 values per day.
#' @param Heading Character String. The heading of the plot. Defaults to NULL.
#' @param YAxisName Character String. The yaxis label text for the plot. Defaults to NULL and returns "value" in the plot.
#' @param LogTransform Boolean. Should the data be presented with a log transformation - useful for some variables such as Turbidity or Chlorophyll
#' @param AttributeName Character String. The name of the attribute to be used if several exist. Defaults to NULL
#'
#' @returns A single ggplot object that can be further transformed/edited/saved
#'
#' @export
#' @examples
#' 
#' nc <- system.file("extdata/turb_reg.nc", package = "RcTools")
#' nc <- stars::read_mdim(nc)
#' 
#' x_plot <- ereefs_dotplot(
#'   nc = nc,
#'   YAxisName = "Turbidity (NTU)", 
#'   Heading = "Dry Tropics", 
#'   LogTransform = TRUE
#' )
#' 

ereefs_dotplot <- function(nc, SubSample = 500, Heading = NULL, YAxisName = NULL, LogTransform = FALSE, AttributeName = NULL){

  #check required argument
  if (missing(nc)){stop("You must supply at least the 'nc' parameter.")}

  #check if single object or list, and convert
  nc <- ereefs_list_safety_check_and_convert(nc)
  
  #continue to check argument types
  if (!is.numeric(SubSample)){stop("You must supply a numeric argument to the 'SubSample' parameter")}
  if (SubSample > 2000){warning("Using a SubSample value greater than 2000 will incur a significant processing cost and reduce function speed.")}
  if (!is.null(YAxisName) & !is.character(YAxisName)){stop("You must supply a character argument to the 'YAxisName' parameter")}
  if (!is.null(Heading) & !is.character(Heading)){stop("You must supply a character argument to the 'Heading' parameter")}
  if (!is.logical(LogTransform)){stop("You must supply a logical (boolean) argument to the 'LogTransform' parameter")}
  if (!is.null(AttributeName) & !is.character(AttributeName)){stop("You must supply a character argument to the 'AttributeName' parameter")}

  #if an attribute name was provided, check if it exists, if so, extract it
  if (!is.null(AttributeName)) {
    if (!any(stringr::str_detect(names(nc), stringr::fixed(AttributeName)))){
      stop("The character provided to the AttributeName parameter must exist within the attributes of the NetCDF object.")
    }
    nc <- nc[AttributeName,,,]
  }

  #check if the netCDF has one attribute or several, if several the user needs to go back and specify
  if (length(names(nc)) > 1){
    stop("The dotplot function should only be applied to netCF objects with a single attribute, if multiple attributes exist in the file supply the name via the 'AttributeName' parameter.")
  }

  #convert the netcdf into a sf object (required by ggplot) and drop geometry
  full_data <- sf::st_drop_geometry(
    sf::st_as_sf(nc, as_points = FALSE, merge = FALSE)
  )

  #take a subset of the data by iterating over each column in the sf object
  sub_sampled_data <- purrr::map_dfc(full_data, \(x) {sample(x, SubSample)})

  #edit the sub sampled data
  sub_sampled_data <- sub_sampled_data |> 
    tidyr::pivot_longer(cols = tidyr::everything(), names_to = "Day", values_to = "Values") |> #pivot longer
    dplyr::mutate(
      Day = as.Date(Day), #format 
      Values = as.numeric(Values), #format
      Season = dplyr::case_when(lubridate::month(Day) > 4 & lubridate::month(Day) < 11 ~ "Dry", T ~ "Wet") #calculate season
    )
  
  #determine roughly how long the data spans
  data_span <- (max(sub_sampled_data$Day) - min(sub_sampled_data$Day))/60

  #calculate the mean value of all data
  mean_val <- mean(sub_sampled_data$Values, na.rm = TRUE)
  
  #create the plot
  p <- ggplot2::ggplot(sub_sampled_data) +
    ggplot2::geom_point(
      ggplot2::aes(x = Day, y = Values, color = Season),
      size = 0.1
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = Day, y = Values),
      method = "gam",
      formula = y~s(x),
      color = "blue",
      se = F
    ) +
    ggplot2::geom_hline(yintercept = mean_val) +
    ggplot2::geom_label(
      data = data.frame(Day = min(sub_sampled_data$Day), Values = mean_val),
      ggplot2::aes(x = Day, y = Values, label = paste("Mean =", round(mean_val, 2))),
      vjust = -0.2, hjust = 0) + 
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(as.numeric(data_span))) +
    ggplot2::labs(x = "Date", y = if (is.null(YAxisName)) {"Value"} else {YAxisName}) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )
  
  if(LogTransform){p <- p + ggplot2::scale_y_log10()}
  if(!is.null(Heading)){p <- p + ggplot2::ggtitle(Heading)}

  #return the plot as the final output of the function
  return(p)

}