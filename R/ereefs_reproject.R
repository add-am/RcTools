#' Convert the eReefs object from curvilinear to a regular grid
#'
#' @param nc A NetCDF (stars) object or list of NetCDF (stars) objects, generally produced by the [ereefs_extract()] function
#'
#' @returns A NetCDF (stars) object or list of NetCDF (stars) objects, with the same values as x, but in a regular grid format
#'
#' @export
#' @examples
#' \dontrun{
#' x_regular <- ereefs_reproject(x_curvilinear)
#' }
ereefs_reproject <- function(nc){

  #if the variable supplied is not a list, convert it to a list to allow it through the map function
  if (!inherits(nc, "list")){nc <- list(nc)}

  #warp the data
  nc <- purrr::map(nc, \(x) {

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
    stars::st_warp(x, reg_stars)

  })

  #if the list output has length one, it can be converted back to a stars object
  if (length(nc) == 1){nc <- nc[[1]]}

  #return the final object
  return(nc)
}