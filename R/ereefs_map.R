#' Mapping eReefs Data
#'
#' @param nc A single NetCDF (stars) object OR a list of NetCDF (stars) objects, generally produced by the [ereefs_extract()] function
#' @param MapType Character String. Defines the type of map produced. One of "Concentration", "True Colour", or "Vector Field" (AKA wind)
#' @param Aggregation Character String. Defines the level of aggregation to apply. Defaults to "Month". Options are "Month", "Season", "Financial", "Annual"
#' @param LegendTitle Character String. The title of the legend. Defaults to the name of the netCDF's attribute
#' @param nrow Numeric String. The number of rows to be used when facetting maps. Defaults to NULL and uses underlying default value
#'
#' @returns A tmap object
#' 
#' @import grid
#'
#' @export
#' @examples
#' 
#' nc <- system.file("extdata/turb_reg.nc", package = "RcTools")
#' nc <- stars::read_mdim(nc)
#' 
#' m <- ereefs_map(
#'   nc = nc,
#'   MapType = "Concentration",
#'   Aggregation = "Annual"
#' )
#' 
ereefs_map <- function(nc, MapType, Aggregation = "Month", LegendTitle = NULL, nrow = NULL){

  #conduct safety checks
  if (any(missing(nc), missing(MapType))){stop("You must supply at least the 'nc' and 'Aggregation' parameters.")}
  if (!is.character(MapType)){stop("You must supply a character string to the 'MapType' parameter")}
  if (!is.null(LegendTitle) & !is.character(LegendTitle)){stop("You must supply a character string to the 'LegendTitle' parameter")}
  if (!is.null(nrow) & !is.numeric(nrow)){stop("You must supply a numeric string to the 'nrow' parameter")}

  #check if single object or list, and convert
  nc <- ereefs_list_safety_check_and_convert(nc)

  #check the map type arguments' value
  if (!MapType %in% c("Concentration", "True Colour", "Vector Field")){
    stop("You must supply one of 'Concentration', 'True Colour', or 'Vector Field' to the 'MapType' parameter.")
  }

  #obtain a qld state outline from the AIMS dataset
  qld <- get(utils::data("gbr_feat", package = "gisaimsr", envir = environment())) |> 
    dplyr::filter(FEAT_NAME %in% c("Mainland", "Island", envir = environment())) |> 
    sf::st_transform("EPSG:9473")

  #obtain a reefs outline from the AIMS dataset
  reefs <- get(utils::data("gbr_feat", package = "gisaimsr")) |> 
    dplyr::filter(FEAT_NAME == "Reef") |> 
    sf::st_transform("EPSG:9473")

  #aggregate the data
  nc <- ereefs_aggregation_helper(nc, agg = Aggregation)

  #if no legend title was provided, use the name of the attributes
  if (is.null(LegendTitle)) {LegendTitle <- names(nc)}

  if (MapType == "Concentration"){

    #create the concentration map
    m <- tmap::tm_shape(qld) +
      tmap::tm_polygons(
        fill = "#99B5B1",
        col = "#7bba9d"
      ) +
      tmap::tm_shape(nc, is.main = TRUE) +
      tmap::tm_raster(
        col = names(nc), 
        col.scale = tmap::tm_scale_continuous(values = ereefs_get_palette(names(nc))),
        col.legend = tmap::tm_legend(
          reverse = TRUE,
          title = LegendTitle,
          na.show = FALSE
        )
      ) +
      tmap::tm_shape(reefs) +
      tmap::tm_borders(
        fill = "grey60",
        fill_alpha = 0.2,
        col = "grey60",
        col_alpha = 0.4
      ) +
      tmap::tm_layout(
        bg.color = "#C1DEEA",
        legend.position = tmap::tm_pos_out("right", "center")
      )
    
    #if the user had a specific number of rows for facetting  
    if (!is.null(nrow)){m <- m + tmap::tm_facets(nrow = nrow)}

  } else if (MapType == "True Colour"){

    #create the true colour map
    m <- tmap::tm_shape(qld) +
      tmap::tm_polygons(
        fill = "#99B5B1",
        col = "#7bba9d"
      ) +
      tmap::tm_shape(nc, is.main = TRUE) +
      tmap::tm_rgb() +
      tmap::tm_shape(reefs) +
      tmap::tm_borders(
        fill = "grey60",
        fill_alpha = 0.2,
        col = "grey60",
        col_alpha = 0.4
      ) +
      tmap::tm_layout(
        bg.color = "#C1DEEA",
        legend.position = tmap::tm_pos_out("right", "center")
      )
    
    #if the user had a specific number of rows for facetting  
    if (!is.null(nrow)){m <- m + tmap::tm_facets(nrow = nrow)}

  } else if (MapType == "Vector Field"){

    #vector fields need to be provided in tabular form, convert the two netcdfs to sf objects
    u_data <- sf::st_as_sf(nc["wind_u (Nm-2)",,,], as_points = T, merge = F)
    v_data <- sf::st_as_sf(nc["wind_v (Nm-2)",,,], as_points = T, merge = F)

    #convert the geometry into lat and lon columns, u and v share the same coordinates so this only needs to be done once
    u_data <- u_data |> 
      dplyr::mutate(
        lat = sf::st_coordinates(geometry)[, "X"],
        lon = sf::st_coordinates(geometry)[, "Y"]
      ) |> 
      sf::st_drop_geometry()

    #drop data for v as well
    v_data <- v_data |> 
      dplyr::mutate(
        lat = sf::st_coordinates(geometry)[, "X"],
        lon = sf::st_coordinates(geometry)[, "Y"]
      ) |> 
      sf::st_drop_geometry()

    #data is then pivoted longer such that all observations share one column
    if (ncol(u_data) > 3){

      u_data <- u_data |> 
        tidyr::pivot_longer(
          cols = -dplyr::matches("lon|lat"), 
          names_to = Aggregation, 
          values_to = "u"
        )
      v_data <- v_data |> 
        tidyr::pivot_longer(
          cols = -dplyr::matches("lon|lat"), 
          names_to = Aggregation, 
          values_to = "v"
        )
      
    } else {

      u_data <- tidyr::pivot_longer(u_data, 1, names_to = Aggregation, values_to = "u")
      v_data <- tidyr::pivot_longer(v_data, 1, names_to = Aggregation, values_to = "v")

    }
    
    #the u and v columns can then be combined into a single tabular dataset
    u_v_data <- dplyr::left_join(u_data, v_data, by = c("lat", "lon", Aggregation))

    #sheer stress magnitude can be recalculated from u and v, rather than trying to pull it through the prior dataframe extraction
    #then wind speed can be back calculated using a few assumptions about standard climatic conditions
    u_v_data <- u_v_data |> 
      dplyr::mutate(
        magnitude = round(sqrt(u^2 + v^2), 4),
        speed = round(sqrt(magnitude/(1.225*1.3e-3))*3.6, 1)
      )
    
    #start the vector field map
    m <- 
      ggplot2::ggplot(
        data = u_v_data,
        ggplot2::aes(x = lon, y = lat)
      ) +
      metR::geom_contour_fill(
        ggplot2::aes(z = speed),
        na.fill = FALSE,
        bins = 70
      ) +
      metR::geom_arrow(
        ggplot2::aes(dx = u * 10, dy = v * 10), #multiply just to make tails longer
        arrow = grid::arrow(angle = 15, length = grid::unit(0.8, "lines"), ends = "last", type = "closed"),
        skip = 6,
        direction = "ccw"
      ) +
      ggplot2::scale_fill_gradientn(
        name = "Wind Speed (km/h)",
        colours = ereefs_get_palette("Wind"),
        limits = c(min(u_v_data$speed), max(u_v_data$speed)),
        breaks = seq(min(u_v_data$speed), max(u_v_data$speed), length.out = 3)) +
      ggplot2::facet_wrap(stats::as.formula(paste("~", Aggregation)))

    #if the user had a specific number of rows for facetting  
    if (!is.null(nrow)){m <- m + ggplot2::facet_wrap(stats::as.formula(paste("~", Aggregation)), nrow = nrow)}
    
  }

  return(m)
  
}