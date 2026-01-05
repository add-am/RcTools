#' Check if object is Stars or list of Stars, Convert if List
#'
#' @param nc A list of NetCDF (stars) objects, generally produced by the [ereefs_extract()] function
#'
#' @returns A single NetCDF (stars) object combined on the time dimension
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' ereefs_list_safety_check_and_convert("Turbidity")
#' }
#' 
ereefs_list_safety_check_and_convert <- function(nc){

  #check if supplied object is not a single netCDF object or a list
  if (!inherits(nc, "stars") & !inherits(nc, "list")){
    stop("You must supply either a single netCDF (stars) object, or a list of netCDF (stars) objects.")
  }

  #if the object is a list...
  if (inherits(nc, "list")){

    #check if it is a list of netCDF objects
    if (any(!purrr::map_lgl(nc, \(nc_object) inherits(nc_object, "stars")))){
      stop("You must supply either a single netCDF (stars) object, or a list of netCDF (stars) objects.")
    }

    #if so, combine objects
    nc_combine <- do.call(c, nc)

    #pull date values associated with each of the datasets
    data_dates <- do.call(c, purrr::map(nc, \(x) stars::st_get_dimension_values(x, "time")))

    #update the date values in the compressed dataset
    stars::st_dimensions(nc_combine)$time$values <- data_dates

    #update the name
    nc <- nc_combine

    #and return the combined object
    return(nc) 

    }

  #otherwise the object would be a stars object and can just be returned
  return(nc)
}