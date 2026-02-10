#' Extract Habitat Data from Datasets on File
#'
#' @param RawPath Character String. The path to where the full habitat files are found
#' @param CropObj Sf Object. The object to be used to crop the files.
#' @param Habitat Character String. Either 'RV' or 'MS' for Riparian Vegetation and Mangrove and Saltmarsh respectively. This 
#' tells the function where to save files.
#'
#' @returns A table object summarising areas and years. Also crops and saves spatial data, and csv versions.
#'
#' @export
#' @examples
#' \dontrun{ #dont run because function takes a long time to process
#' 
#' p1 <- "path/to/folder"
#' 
#' n3_region <- build_n3_region()
#' 
#' all_data <- extract_habitat(p1, n3_region, "RV")
#' }
#' 
extract_habitat <- function(RawPath, CropObj, Habitat){

  #check required arguments
  if (any(missing(RawPath), missing(CropObj), missing(Habitat))){
    stop("You must supply the 'RawPath', 'CropObj', and 'Habitat', parameters.")
  }
  
  #continue to check argument types
  if (!is.character(RawPath)){stop("You must supply a character string to the 'RawPath' parameter.")}
  if (!inherits(CropObj, "sf")){stop("You must supply an sf object to the 'CropObj' parameter.")}
  if (!is.character(Habitat)){stop("You must supply an sf object to the 'Habitat' parameter.")}

  if (!Habitat %in% c("RV", "MS")){
    stop("You must supply either 'RV' (Riparian Vegetation) or 'MS' (Mangrove and Saltmarsh) to the 'Habitat' parameter.")}
  
  #build the relevant output directory
  if (Habitat == "RV"){
    dir.create(glue::glue("{dirname(RawPath)}/riparian_vegetation/"))
    OutPath <- glue::glue("{dirname(RawPath)}/riparian_vegetation/")
  }
  if (Habitat == "MS"){
    dir.create(glue::glue("{dirname(RawPath)}/mangroves_and_saltmarsh/"))
    OutPath <- glue::glue("{dirname(RawPath)}/mangroves_and_saltmarsh/")
  }

  #list all files in the raw folder
  full_files <- list.files(RawPath, full.names = TRUE)

  #for each file in the raw folder
  purrr::walk(full_files, \(x) {

    #extract just the file name
    x_cropped_path <- x |> 
      stringr::str_extract("[^/]+$") |> 
      stringr::str_replace("_[^_]*$", "") 
    
    #create a path to where the cropped version of the data will/should be saved
    x_cropped_path <- paste0(OutPath, x_cropped_path, "_cropped.gpkg")
    
      #if the cropped version does not exist
      if (!file.exists(x_cropped_path)){

        #open the data
        x_open <- sf::st_read(x)

        #crop the data
        x_cropped <- x_open |>
          sf::st_transform(sf::st_crs(CropObj)) |> 
          sf::st_intersection(CropObj)
        
        #save the data
        sf::st_write(x_cropped, x_cropped_path)

      }
  })

  #list all files in the processed folder with the .gpkg extension
  cropped_files_gpkg <- list.files(OutPath, full.names = TRUE, pattern = ".gpkg")

  #for each of the cropped .gpkg files in the processed folder
  purrr::walk(cropped_files_gpkg, \(x) {

    #create a path to a csv version
    x_csv_path <- str_replace(x, ".gpkg", ".csv")

    #if the csv version does not exist
    if (!file.exists(x_csv_path)){

      #open
      x_open <- sf::st_read(x)

      #calculate the area of everything
      x_sf <- x_open |> 
        sf::st_transform("EPSG:7855") |> 
        mutate(area_m2 = sf::st_area(geom))|> 
        mutate(area_km2 = units::set_units(area_m2, "km2"))

      #extract everything but geometry
      x_tbl <- sf::st_drop_geometry(x_sf)

      #save the table
      write_csv(x_tbl, x_csv_path)
    }
  })

  #list all files in the processed folder with the .csv extension
  cropped_files_csv <- list.files(OutPath, full.names = TRUE, pattern = ".csv")

  #for each of the cropped .csv files in the processed folder
  all_data_tbl <- purrr::map(cropped_files_csv, \(x) {

    #figure out the data source (i.e. a simplified version of the name)
    data_source <- x |> 
      str_extract("[^/]+$") |> 
      str_replace("_[^_]*$", "")

    #open and edit the data
    x_open <- x |> 
      read_csv() |> 
      clean_names() |> 
      group_by(re1) |> 
      summarise(area_km2 = sum(area_km2)) |> 
      mutate(source = data_source)

    #return the data
    return(x_open)
      
  })

  #combine the list of dataframes into one
  all_data_tbl <- dplyr::bind_rows(all_data_tbl)

  #return the data as the final output of the function
  return(all_data_tbl)

}

