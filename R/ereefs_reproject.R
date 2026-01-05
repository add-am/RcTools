#' Convert the eReefs object from curvilinear to a regular grid
#'
#' @param nc A single NetCDF (stars) object, generally produced by the [ereefs_extract()] function
#'
#' @returns A single NetCDF (stars) object, with the same values as x, but in a regular grid format
#'
#' @export
#' @examples
#' 
#' load(system.file("extdata/turb_curvi.RData", package = "RcTools"))
#' 
#' 
#' nc_reg <- ereefs_reproject(turb_curvi)
#' 
ereefs_reproject <- function(nc){
    
  #update the crs on the data (move from lat long to meters) (this is the Australian Albers Equal projection)
  nc <- nc |> sf::st_transform("EPSG:9473")

  #convert our curvilinear object into just a bbox
  curvilinear_bbox <- nc |> 
    sf::st_bbox() |>
    sf::st_as_sfc()

  #get a linear grid target with the same dimensions (number of cells) as our curvilinear grid 
  reg_stars <- stars::st_as_stars(
    curvilinear_bbox, #using the bbox to provide the xmin, xmax etc., 
    nx = dim(nc)[[1]], #and the dimensions to provide the x and y count. 
    ny = dim(nc)[[2]], 
    values = NA_real_) #Fill each cell with NA

  #run st warp, it requires a curvilinear object, and a regular object as a target
  nc <- stars::st_warp(nc, reg_stars)

  #return the final object
  return(nc)
}