#' Mapping eReefs Data
#'
#' @param nc A netCDF (stars) object, usually produced by the [ereefs_extract] and [ereefs_reproject] functions
#'
#' @returns A tmap object
#'
#' @export
#' @examples
#' \dontrun{
#' m <- ereefs_map(nc)
#' }
ereefs_map <- function(nc){

  #note for later, going to lean into using the rgb() plain call as the set up around the dataset that works with this
  #function is much better (i.e related to saving, reloading, and being consistent with variable/attribute names)
  tmap::tm_shape(nc) + tmap::tm_rgb()
  
  
  
}