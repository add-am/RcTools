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

  #data needed: 
  #n3_region (a land file and a marine file), provide externally?? Or allow it to be added after? It should be a top layer anyway
  #qld outline (can come from the AIMS dataset)
  #reef outline (can come from the AIMS dataset)

  #data privded
  #netcdf object, or a list of netcdf objects if looking for wind??
  #lets just start with the one

  #types of map to create
  #rgb true colour
  #concentration
  #vector field

  #extras to apply to map - I would need to devise a clean method of having all these options
  #single map
  #faceted monthly
  #faceted seasonally
  #faceted annually
  #faceted by finacial year?

  #steps:
  



  #note for later, going to lean into using the rgb() plain call as the set up around the dataset that works with this
  #function is much better (i.e related to saving, reloading, and being consistent with variable/attribute names)
  tmap::tm_shape(nc) + tmap::tm_rgb()
  
  
  
}