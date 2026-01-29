#' Extract, Crop, and Combine Air Temperature Data
#'
#' @param CropObj An sf object. Generally the object produced by [build_n3_region()]. Defines the area in which to crop data
#' @param MinPath A character string. Defines the full path to the folder in which all min temp sub folders are stored.
#' Generally this will be user/.../.../raw/tmin2/
#' @param MaxPath A character string. Defines the full path to the folder in which all max temp sub folders are stored.
#' Generally this will be user/.../.../raw/tmax2/
#'
#' @returns A list of three netCDF objects. Specifically a min temp, max temp, and mean temp netCDF
#'
#' @export
#' @examples
#' \dontrun{ #dont run because function takes a while
#' 
#' min_path <- "C:/Users/path/to/data/raw/tmin2"
#' max_path <- "C:/Users/path/to/data/raw/tmax2"
#' 
#' n3_region <- build_n3_region()
#' 
#' nc <- extract_air_temp(n3_region, min_path, max_path)
#' 
#' }
#' 
extract_air_temp <- function(CropObj, MinPath, MaxPath){

  #check required argument (all of them)
  if (any(missing(CropObj), missing(MinPath), missing(MaxPath))){stop("You must supply all parameters.")}

  #check argument types
  if (!inherits(CropObj, "sf")){stop("You must supply an sf object to the 'CropObj' parameter")}
  if (!is.character(MinPath)){stop("You must supply a character argument to the 'MinPath' parameter")}
  if (!is.character(MaxPath)){stop("You must supply a character argument to the 'MaxPath' parameter")}

  #calculate a buffer value
  buff_val <- if(sf::st_is_longlat(sf::st_crs(CropObj))){0.1} else {15000}

  #buffer the supplied sf object slightly
  buffed_obj <- CropObj |> 
    sf::st_union() |> 
    sf::st_buffer(buff_val)

  #join the min and max paths into a list to iterate on
  min_max_paths <- list(MinPath, MaxPath)
 
  #list all the available files
  all_file_paths <- purrr::map(min_max_paths, \(x) {list.files(x, full.names = TRUE, recursive = TRUE)})

  #create a second list of equal length for where to save cropped files
  all_cropped_paths <- purrr::map(all_file_paths, \(x) {
    x |> 
      stringr::str_replace(".nc", "_cropped.nc") |> 
      stringr::str_replace("raw", "processed")
  })

  #flatten each of the objects into single long lists
  all_file_paths_flat <- purrr::flatten(all_file_paths)
  all_cropped_paths_flat <- purrr::flatten(all_cropped_paths)

  #for each file, crop and resave
  purrr::walk2(all_file_paths_flat, all_cropped_paths_flat, \(x, y) {

    if (!file.exists(y)){

      nc <- stars::read_stars(x)

      nc <- stars::st_warp(nc, crs = st_crs(buffed_obj))

      if(!dir.exists(dirname(y))){dir.create(dirname(y), recursive = TRUE)}

      nc_crop <- sf::st_crop(nc, buffed_obj)

      stars::write_stars(nc_crop, y)
    }
  })

  #once all files have been cropped, read in the cropped files - division by min and max is required for this map
  all_at_data <- purrr::map(all_cropped_paths, \(x) {

    #read the data in
    all_layers <- stars::read_stars(x, along = "time")

    #extract the vals that are stored as "time"
    time_vals <- stars::st_get_dimension_values(all_layers, "time")

    #strip the text down to a useable amount
    time_vals <- stringr::str_sub(time_vals, -8)

    #convert them into actual time values
    time_dates <- as.Date(paste0(time_vals, "01"), format = "%Y%m%d")

    #put the real time values back into the data
    stars::st_dimensions(all_layers)$time$values <- time_dates

    #extract all dimensions
    dims <- stars::st_dimensions(all_layers) 

    #update the reference system for time
    dims$time$refsys <- "Date" 

    #then update the original object
    stars::st_dimensions(all_layers) <- dims

    #return the data
    return(all_layers)
  })

  #extract the raw array values and take the mean manually (for speed)
  mean_arr <- (all_at_data[[1]][[1]] + all_at_data[[2]][[1]])/2

  #build a stars object using the mean array and dimensions of the original object and put it into the list
  all_at_data[[3]] <- stars::st_as_stars(mean_arr, dimensions = stars::st_dimensions(all_at_data[[1]]))

  #update the names of all datasets
  names(all_at_data[[1]]) <- "min"
  names(all_at_data[[2]]) <- "max"
  names(all_at_data[[3]]) <- "mean"

  #return the list of three datasets
  return(all_at_data)

}