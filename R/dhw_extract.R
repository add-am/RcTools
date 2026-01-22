#' Extract Degree Heating Week Data from NOAA's API
#'
#' @param FullPath Character String. A path arguement to the location where each full file should be saved and retrived.
#' @param CroppedPath Character String. A path arguement to the location where each cropped file should be saved and retrived.
#' @param CropObj Sf Object. An sf object used to define the area in which data is to be cropped to. Generally,
#' the n3_region object from the [build_n3_region()] function is used.
#'
#' @returns A stars netCDF object AND saves individual laers to file. (Note, this is because each file can only be downloaded
#' individually. If just the single netCDF was returned, every layer would need to be re-downloaded once a new layer is 
#' published. Instead, only the new layer is downloaded, and the single netCDF is rebuilt locally).
#'
#' @export
#' @examples
#' \dontrun{ #dont run because function takes a long time to process
#' 
#' p1 <- "path/to/folder/raw/"
#' p2 <- "path/to/folder/processed/"
#' 
#' n3_region <- build_n3_region()
#' 
#' dhw_extract(p1, p2, n3_region)
#' }
#' 
dhw_extract <- function(FullPath, CroppedPath, CropObj){

  #check required arguments
  if (any(missing(FullPath), missing(CroppedPath), missing(CropObj))){
    stop("You must supply the 'FullPath', 'CroppedPath', and 'CropObj', parameters.")
  }
  
  #continue to check argument types
  if (!is.character(FullPath)){stop("You must supply a character string to the 'FullPath' parameter.")}
  if (!is.character(CroppedPath)){stop("You must supply a character string to the 'CroppedPath' parameter.")}
  if (!inherits(CropObj, "sf")){stop("You must supply an sf object to the 'CropObj' parameter.")}
    
  #convert the provided sf object into a bbox
  target_bbox <- CropObj |> 
    sf::st_transform("EPSG:4326") |> 
    sf::st_bbox() |> 
    sf::st_as_sfc()

  #turn of spherical geometry  
  sf::sf_use_s2(FALSE)

  #build a base url
  base_url <- "https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0"

  #create a vector between 1986 and the current year
  date_vect <- 1986:format(Sys.Date(), "%Y")

  #build an equal length vector of file name character strings to iterate on
  raw_file_names <- glue::glue("{FullPath}/dhw_{date_vect}.nc")

  #build a second vector of file names for the cropped version
  cropped_file_names <- glue::glue("{CroppedPath}/cropped_dhw_{date_vect}.nc")

  #drop the current year (its url is built differently)
  date_vect <- head(date_vect, -1)

  #then create urls for each year between 1986 and the present year (minus 1)
  url_names <- glue::glue("{base_url}/annual/ct5km_dhw-max_v3.1_{date_vect}.nc")

  #extract the current year and the current month and day (minus 2 to account for a data upload delay)
  cy <- format(Sys.Date(), "%Y")
  md <- format(Sys.Date()-2, "%m%d")

  #build the final url for the current year (needs ytd data instead) and bind it to a full vector of urls
  url_names <- c(
    url_names,
    glue::glue("{base_url}/daily/year-to-date/ct5km_dhw-max-ytd_v3.1_{cy}{md}.nc")
  )

  #map over each of the vectors, if the file does not exists locally, download it
  purrr::walk2(raw_file_names, url_names, \(x,y) {if(!file.exists(x)){utils::download.file(y, x, mode = "wb")}})

  #map over each of the vectors, if the file does not exists locally, crop it
  purrr::walk2(raw_file_names, cropped_file_names, \(x, y) {

    if(!file.exists(y)){

      #read
      nc <- stars::st_warp(stars::read_stars(x), crs = "EPSG:4326")

      #crop
      nc_cropped <- sf::st_crop(nc, target_bbox)

      #save
      stars::write_stars(nc_cropped, y)

    }
  })

  #map over the list of cropped file names, loading and stacking each layer
  full_dhw_file <- stars::read_stars(cropped_file_names, along = "time")

  #extract the vals that are stored as "time"
  time_vals <- stars::st_get_dimension_values(full_dhw_file, "time") 

  #convert them into actual time values
  time_dates <- as.Date(paste0(time_vals, "-06-01"))

  #put the real time values back into the data
  stars::st_dimensions(full_dhw_file)$time$values <- time_dates   

  #extract all dimensions
  dims <- stars::st_dimensions(full_dhw_file) 

  #update the reference system for time
  dims$time$refsys <- "Date" 

  #then update the original object
  stars::st_dimensions(full_dhw_file) <- dims

  #return the object
  return(full_dhw_file)
}