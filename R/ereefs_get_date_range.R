#' Return the Model's Min and Max Dates
#'
#' @param Model Character string. Defaults to "catalog" however can also accept a premade url
#'
#' @returns Character string. Describes the min and max dates for the specific model
#'
#' @export
#' @examples
#' date_range <- ereefs_get_date_range()
#' 
ereefs_get_date_range <- function(Model = "catalog"){

  #check argument types
  if (!is.character(Model)){stop("You must supply a character argument to the 'Model' parameter")}
  
  #check model
  Model <- ereefs_input_selection(Model)

  #open the nc file
  nc <- ereefs::safe_nc_open(Model)
  
  #get a vector of all times
  ds <- as.Date(floor(ereefs::safe_ncvar_get(nc, "time")), origin = as.Date("1990-01-01"))

  #close the nc file
  ncdf4::nc_close(nc)

  #print a message to the user
  paste0("The oldest date for this model is: ", min(ds), ". The newest date for this model is ", max(ds))

}