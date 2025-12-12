#' Convert the eReefs object from curvilinear to a regular grid
#'
#' @param x A NetCDF (stars) object, generally produced by the [ereefs_extract()] function
#'
#' @returns A NetCDF (stars) object with the same values as x, but in a regular grid format
#'
#' @export
#' @examples
#' \dontrun{
#' x_regular <- ereefs_reproject(x_curvilinear)
#' }
ereefs_reproject <- function(x){

  #update the crs on the data (move from lat long to meters) (this is the Australian Albers Equal projection)
  x <- x |> sf::st_transform("EPSG:9473")

  #convert our curvilinear object into just a bbox
  curvilinear_bbox <- x |> 
    sf::st_bbox() |>
    sf::st_as_sfc()

  #get a linear grid target with the same dimensions (number of cells) as our curvilinear grid 
  reg_stars <- stars::st_as_stars(
    curvilinear_bbox, #using the bbox to provide the xmin, xmax etc., 
    nx = dim(x)[[1]], #and the dimensions to provide the x and y count. 
    ny = dim(x)[[2]], 
    values = NA_real_) #Fill each cell with NA

  #run st warp, it requires a curvilinear object, and a regular object as a target
  warped_data <- stars::st_warp(x, reg_stars)

  #return this object
  warped_data
}