#' Crop A Raster and Increase its Resolution at the Same Time
#'
#' @param nc A single NetCDF (stars) object, usually produced as part of the Climate workflow.
#' @param CropObj An sf object that defines the area in which data is to be cropped too.
#' @param DissaggFactor Numeric string. Defines the level of disaggregation to apply to the data (i.e. how many additional cells to create). 
#' Defaults to five, i.e. one cell will become five cells.
#'
#' @returns A singele NetCDF (stars) object.
#'
#' @export
#' @examples
#' 
#' nc <- stars::read_mdim(system.file("extdata/turb_reg.nc", package = "RcTools"))
#' area <- sf::st_read(system.file("exdata/example_box.gpkg"), package = "RcTools")
#' 
#' nc_high_res <- high_res_crop(nc, area, 5)
#' 
high_res_crop <- function(nc, CropObj, DissaggFactor = 5){

  #determine the type of buffer to use (i.e meters or longlat)
  buff_val <- if(sf::st_is_longlat(sf::st_crs(CropObj))){0.1} else {10000}

  #create a buffered version of the supplied object
  buffed_obj <- CropObj |> 
    sf::st_union() |> 
    sf::st_buffer(buff_val)

  #crop the supplied raster with close proximity
  initial_crop <- sf::st_crop(nc, buffed_obj)

  #increase the resolution of the initially cropped object
  high_resolution <- sf::st_warp(
    initial_crop, 
    sf::st_as_stars(
      st_bbox(initial_crop),
      dx = (1/DisaggFactor) * sf::st_dimensions(x)$x$delta, 
      dy = (1/DisaggFactor) * sf::st_dimensions(x)$y$delta
    ), 
    method = "bilinear", 
    use_gdal = TRUE
  )

  #then mask again to get a precise border with much higher resolution
  final_raster <- sf::st_crop(high_resolution, CropObj)

  #return the object
  return(final_raster)
}