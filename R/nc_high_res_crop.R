#' Crop A Raster and Increase its Resolution at the Same Time
#'
#' @param nc A single NetCDF (stars) object, usually produced as part of the Climate workflow.
#' @param CropObj An sf object that defines the area in which data is to be cropped too.
#' @param DisaggFactor Numeric string. Defines the level of disaggregation to apply to the data (i.e. how many additional cells to create). 
#' Defaults to five, i.e. one cell will become five cells.
#'
#' @returns A singele NetCDF (stars) object.
#'
#' @export
#' @examples
#' 
#' nc <- stars::read_mdim(system.file("extdata/turb_reg.nc", package = "RcTools"))
#' area <- sf::st_read(system.file("exdata/example_box.gpkg", package = "RcTools"))
#' 
#' nc_high_res <- high_res_crop(nc, area, 5)
#' 
nc_high_res_crop <- function(nc, CropObj, DisaggFactor = 5){

  #determine the type of buffer to use (i.e meters or longlat)
  buff_val <- if(sf::st_is_longlat(sf::st_crs(CropObj))){0.1} else {10000}

  #create a buffered version of the supplied object
  buffed_obj <- CropObj |> 
    sf::st_union() |> 
    sf::st_buffer(buff_val)

  #crop the supplied raster with close proximity
  initial_crop <- sf::st_crop(nc, buffed_obj)

  #if the supplied data has only one time step, give it another (one timestep causes strange error)
  if (dim(nc)[[3]] == 1){
    
    #duplicate data
    initial_crop_2 <- initial_crop

    #edit the timestep on the duplicate
    stars::st_dimensions(initial_crop_2)$time$values <- stars::st_dimensions(initial_crop)$time$values+1

    #merge the data to make a new version with two timesteps
    initial_crop <- c(initial_crop, initial_crop_2, along = "time")
  }

  #build a target layout
  target_nc <- initial_crop |> 
    sf::st_bbox() |> 
    stars::st_as_stars(
      dx = (1/DisaggFactor) * stars::st_dimensions(initial_crop)$x$delta, 
      dy = (1/DisaggFactor) * stars::st_dimensions(initial_crop)$y$delta)

  #then warp the data into the target objectobject
  high_resolution <- stars::st_warp(initial_crop, target_nc, method = "bilinear", use_gdal = TRUE)

  #and properly add the value back in
  stars::st_dimensions(high_resolution)$band$values <- stars::st_dimensions(initial_crop)$time$values

  #if data is supposed to have one timestep, make this true
  if (dim(nc)[[3]] == 1){high_resolution <- high_resolution[,,,1]}

  #update the last dimension to have the correct name
  high_resolution <- stars::st_set_dimensions(high_resolution, "band", names = "time")

  #then mask again to get a precise border with much higher resolution
  final_raster <- sf::st_crop(high_resolution, CropObj)

  #return the object
  return(final_raster)
}