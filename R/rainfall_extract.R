
#' Extract Rainfall Data from BOM AWO API
#'
#' @param Region An sf object
#' @param StartDate A character string in the format of YYYY-MM-DD. Defaults to "1911-01-31" (earliest value)
#' @param EndDate A character string in the format of YYYY-MM-DD. Defaults to newest value
#'
#' @returns A stars  netCDF object
#'
#' @export
#' @examples
#' \dontrun{ #dont run because example takes a while
#' n3_region <- build_n3_region()
#' 
#' rain_data <- rainfall_extract(n3_region, "2022-01-01", "2022-02-01")
#' }
#' 
rainfall_extract <- function(Region, StartDate = NULL, EndDate = NULL){

  #check required arguments
  if (missing(Region)){stop("You must supply at least the 'Region', parameter.")}
  
  #continue to check argument types
  if (!inherits(Region, "sf")){stop("You must supply an sf object to the 'Region' parameter.")}
  if (!is.null(StartDate) & !is.character(StartDate)){stop("You must supply a character string to the 'StartDate' parameter or leave it blank.")}
  if (!is.null(EndDate) & !is.character(EndDate)){stop("You must supply a character string to the 'EndDate' parameter or leave it blank.")}

  #if dates supplied, confirm they can be converted
  if (!is.null(StartDate)){StartDate <- as.Date(StartDate)}
  if (!is.null(EndDate)){EndDate <- as.Date(EndDate)}
  
  #set the url to the data that we are going to access
  url <- "https://thredds.nci.org.au/thredds/dodsC/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/rain_day.nc"

  #open the url (this contains the bare essential information about the dataset)
  nc <- ncdf4::nc_open(url)

  #get information about the time dimension
  time_vals <- as.Date(ncdf4::ncvar_get(nc, "time"), origin = "1900-01-01")

  #if dates were not supplied, update their values
  if (is.null(StartDate)){StartDate <- min(time_vals)}
  if (is.null(EndDate)){EndDate <- max(time_vals)}

  #find the index of the value that closest matches each of the time values (1 min and 1 max)
  time_start <- which.min(abs(time_vals - StartDate))
  time_count <- which.min(abs(time_vals - EndDate)) - time_start
        
  #get information about the lat and long dimensions
  lon <- ncdf4::ncvar_get(nc, "longitude")
  lat <- ncdf4::ncvar_get(nc, "latitude")

  #convert the provided sf object into a bbox
  target_bbox <- Region |> 
    sf::st_transform("EPSG:7844") |> 
    sf::st_buffer(0.1) |>
    sf::st_bbox()   

  #find the index of the values that closest matche the bbox lon vals, then count how many vals are between then 
  lon_start <- which.min(abs(lon - target_bbox["xmin"]))
  lon_count <- which.min(abs(lon - target_bbox["xmax"])) - lon_start

  #find the index of the values that closest matche the bbox lat vals, then count how many vals are between then
  lat_start <- which.min(abs(lat - target_bbox["ymax"])) #this is reversed (use ymax) because negative values
  lat_count <- which.min(abs(lat - target_bbox["ymin"])) - lat_start

  #use the time, lon, and lat index values to pull the data
  target_data <- stars::read_ncdf(
    url,
    var = "rain_day",
    proxy = FALSE,
    ncsub = cbind(start = c(lon_start, lat_start, time_start), 
    count = c(lon_count, lat_count, time_count))
  )

  #warp the data to a projected crs
  target_data <- stars::st_warp(target_data, crs = "EPSG:7855")

  return(target_data)

}