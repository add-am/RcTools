#' Extract Land Use Data from a collection of Datasets on File
#'
#' @param RawPath Character String. A path arguement to the location where each full file should be retrived from.
#' @param CroppedPath Character String. A path arguement to the location where each cropped file should be saved and retrived.
#' @param CropObj Sf Object. An sf object used to define the area in which data is to be cropped to. Generally,
#' the n3_region object from the [build_n3_region()] function is used.
#'
#' @returns A single sf object.
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
#' lu_data <- extract_land_use(p1, p2, n3_region)
#' }
#' 
extract_land_use <- function(RawPath, CroppedPath, CropObj){

  #check required arguments
  if (any(missing(RawPath), missing(CroppedPath), missing(CropObj))){
    stop("You must supply the 'RawPath', 'CroppedPath', and 'CropObj', parameters.")
  }
  
  #continue to check argument types
  if (!is.character(RawPath)){stop("You must supply a character string to the 'RawPath' parameter.")}
  if (!is.character(CroppedPath)){stop("You must supply a character string to the 'CroppedPath' parameter.")}
  if (!inherits(CropObj, "sf")){stop("You must supply an sf object to the 'CropObj' parameter.")}

  #list all files in the raw folder
  all_files <- list.files(RawPath, full.names = TRUE)

  #for each file:
  all_data_list <- purrr::map(all_files, \(x){

    #list_all layer names
    layers <- sf::st_layers(x)$name

    #keep only layers with names that contain "_LU_" (only applicable if more than one layer exists)
    if (length(layers) > 1){layers <- layers[stringr::str_detect(layers, "_LU_")]}

    #for all the remaining layers, open them
    data <- purrr::map(layers, \(y) sf::st_read(x, layer = y))

    #name the output
    names(data) <- layers

    #and return
    return(data)

  })

  #flatten any lists of lists
  all_data_list <- purrr::flatten(all_data_list)

  #this flattened list also tells us how many cropped datasets should exist. Thus build a list of cropped dataset paths
  cropped_df_paths <- glue::glue("{CroppedPath}/{names(all_data_list)}.gpkg")

  #remove curvilinear geometries, crop, and save
  purrr::walk2(all_data_list, cropped_df_paths, \(x,y) {

    if(!file.exists(y)){

      #standardise column name format and length (must be 10 characters or less)
      x <- x |> 
        dplyr::rename_with(tolower, dplyr::everything()) |> 
        dplyr::select(dplyr::matches("Primary|Secondary|Tertiary")) |> 
        dplyr::rename_with(~ stringr::str_replace(.x, "_.*", ""))    

      #rename the shape column if it exists
      if ("shape" %in% colnames(x)){x <- dplyr::rename(x, geom = shape)}

      #separate the data into "standard" geometries - i.e. polygons
      standard_geoms <- x |> 
        dplyr::filter(grepl("POLYGON", sf::st_geometry_type(geom))) |> 
        sf::st_make_valid()

      #and non-standard geometries - e.g.. multisurfaces and curvilinear objects
      non_standard_geoms <- x |> 
        dplyr::filter(grepl("MULTISURFACE", sf::st_geometry_type(geom)))
        
      #save non-standard geoms to be edited
      sf::st_write(
        non_standard_geoms, 
        glue::glue("{CroppedPath}/temporary_storage.gpkg"), 
        delete_dsn = TRUE
      )
        
      #use the gdalUtilies to open edit and save the file (it auto saves as a folder containing a shapefile)
      gdalUtilities::ogr2ogr(
        glue::glue("{CroppedPath}/temporary_storage.gpkg"), 
        glue::glue("{CroppedPath}/temporary_storage"),
        explodecollections = TRUE,
        nlt = 'CONVERT_TO_LINEAR', 
        overwrite = TRUE
      )
        
      #read back in the edited file
      fixed_geoms <- sf::st_read(glue::glue("{CroppedPath}/temporary_storage/temporary_storage.shp")) |> 
        dplyr::rename(geom = geometry) |> 
        sf::st_make_valid()
        
      #join the fixed data onto the standard data
      x <- rbind(standard_geoms, fixed_geoms)

      #transfrom the data
      x <- sf::st_transform(x, sf::st_crs(CropObj))

      #crop the data
      x <- sf::st_intersection(x, CropObj)

      #save the data
      sf::st_write(x, y)
    }
  })

  #remove the temporary folder and file
  unlink(glue::glue("{CroppedPath}/temporary_storage/"), recursive = TRUE)
  unlink(glue::glue("{CroppedPath}/temporary_storage.gpkg"))

  #bind all data into a single object, extract year, and remove overlaps between WT and DT
  all_data_df <- cropped_df_paths |> 
    purrr::map(\(x) sf::st_read(x) |> dplyr::mutate(Name = stringr::str_replace(x, ".*\\/", ""))) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(Year = stringr::str_extract(Name, "\\d{4}")) |> 
    dplyr::filter(
      !(Region %in% c("Dry Tropics", "Burdekin") & Year == "2015"),
      !(Region == "Wet Tropics" & Year == "2016")) |> 
    name_cleaning()

  #return the object
  return(all_data_df)
}