#' Extract Sea Surface Temperature Data from NOAA's API
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
#' sst_extract(p1, p2, n3_region)
#' }
#' 
sst_extract <- function(FullPath, CroppedPath, CropObj){

  #check required arguments
  if (any(missing(FullPath), missing(CroppedPath), missing(CropObj))){
    stop("You must supply the 'FullPath', 'CroppedPath', and 'CropObj', parameters.")
  }
  
  #continue to check argument types
  if (!is.character(FullPath)){stop("You must supply a character string to the 'FullPath' parameter.")}
  if (!is.character(CroppedPath)){stop("You must supply a character string to the 'CroppedPath' parameter.")}
  if (!inherits(CropObj, "sf")){stop("You must supply an sf object to the 'CropObj' parameter.")}
  
  #buffer the provided object then convert into a bbox
  target_bbox <- CropObj |> 
    sf::st_transform("EPSG:4326") |> 
    sf::st_bbox() |> 
    sf::st_as_sfc() |>
    sf::st_as_sf() |>
    sf::st_buffer(0.1)
  
  #turn of spherical geometry  
  sf::sf_use_s2(FALSE)

  #build the base url
  base_url <- "https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/monthly/"

  #define the most recent completed month (data is approximately 1 month delayed)
  end_date <- lubridate::floor_date(as.Date(format(Sys.time(), "%Y-%m-%d")), unit = "month") %m-% months(1)

  #build a vector of YYYYMM character strings from 1985 to "now"
  date_vect <- format(seq(as.Date("1985-01-01"), end_date, by = "month"), "%Y%m")

  #build an equal length vector of YYYY character strings
  folder_vect <- stringr::str_sub(date_vect, end = -3)

  #build an equal length vector of file name character strings to iterate on
  raw_file_names <- glue::glue("{FullPath}/sst_{date_vect}.nc")

  #build a second vector of file names for the cropped version
  cropped_file_names <- glue::glue("{CroppedPath}/cropped_sst_{date_vect}.nc")

  #build an equal length vector of url name character strings to iterate on
  url_names <- glue::glue("{base_url}{folder_vect}/ct5km_sst-mean_v3.1_{date_vect}.nc")

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

  #map over the list of cropped file names
  full_sst_file <- stars::read_stars(cropped_file_names, along = "time")

  #extract the vals that are stored as "time"
  time_vals <- stars::st_get_dimension_values(full_sst_file, "time") 

  #convert them into actual time values
  time_dates <- as.Date(paste0(time_vals, "01"), format = "%Y%m%d")

  #put the real time values back into the data
  stars::st_dimensions(full_sst_file)$time$values <- time_dates   

  #extract all dimensions
  dims <- stars::st_dimensions(full_sst_file) 

  #update the reference system for time
  dims$time$refsys <- "Date" 

  #then update the original object
  stars::st_dimensions(full_sst_file) <- dims

  #return the object
  return(full_sst_file)
}

