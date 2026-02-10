#write the initial generalised function to calculate before and after codes
#' Compare how a dataset changes over time
#'
#' @param sf An sf object. Generally the object should contain polygons geometries, and must contain at least 2 years of data.
#' @param Col Character string. The name of the column from which to extract information about the geometry
#' @param StartYear Numeric string. The first year of data to use. Must exist within the sf object.
#' @param EndYear Numeric string. The second year of data to use. Must exist within the sf object.
#' @param Resolution Numeric string. The number of grid cells to create along the x and y planes - will for a square grid. Defaults to 1000
#'
#' @returns An sf object
#'
#' @export
#' @examples
#' 
#' polygons <- list(
#'   rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)), 
#'   rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)),
#'   rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1)),
#'   rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1)))
#' 
#' polygons <- sf::st_sfc(lapply(polygons, function(x) sf::st_polygon(list(x))))
#' 
#' poly_sf <- dplyr::bind_rows(
#'   sf::st_sf(Year = 2000, Id = c("a", "b", "c", "d"), geometry = polygons, crs = 4326), 
#'   sf::st_sf(Year = 2020, Id = rep("a",4), geometry = polygons, crs = 4326))
#' 
#' change <- sf_change_over_time(poly_sf, "Id", 2000, 2020)
#' 
#' tmap::tm_shape(change) + tmap::tm_polygons(fill = "ValueChange")
#' 
sf_change_over_time <- function(sf, Col, StartYear, EndYear, Resolution = 1000){

  #check required arguments
  if (any(missing(sf), missing(Col), missing(StartYear), missing(EndYear))){
    stop("You must supply at least the 'sf', 'Col', 'StartYear' and 'EndYear' parameters.")}

  #check required arguement types
  if (!inherits(sf, "sf")){stop("The object supplied to the 'sf' parameter must be of type sf.")}
  if (!is.character(Col)){stop("The object supplied to the 'Col' parameter must be of character type.")}
  if (!is.numeric(StartYear)){stop("The object supplied to the 'StartYear' parameter must be of numeric type.")}
  if (!is.numeric(EndYear)){stop("The object supplied to the 'EndYear' parameter must be of numeric type.")}
  if (!is.numeric(Resolution)){stop("The object supplied to the 'Resolution' parameter must be of numeric type.")}
  
  #figure out the dimensions of the before/after by inspecting the length of the unique values in the column
  length_of_unique_values <- length(unique(sf[[Col]]))
  
  #create the pre vector based on the size of the length of unique values
  pre_vector <- purrr::map_dbl(1:length_of_unique_values, \(x) (x*length_of_unique_values)+1)
  
  #create the pre change dataset
  pre_value_df <- data.frame(
    PreValue = sort(unique(sf[[Col]])),
    PreCode = pre_vector)
  
  #create the post change dataset
  post_value_df <- data.frame(
    PostValue = sort(unique(sf[[Col]])),
    PostCode = 1:length_of_unique_values)
  
  #merge the two tables (essentially cross multiplies the rows)
  value_change_key <- merge(pre_value_df, post_value_df) |> 
    tidyr::unite("ValueChange", PreValue, PostValue, sep = " to ", remove = FALSE) |> 
    dplyr::mutate("CodeChange" = PreCode - PostCode)

  #remove geometrycollection data types as they don't place nice
  sf <- sf |> 
    sf::st_collection_extract("POLYGON")
  
  #write the before codes to the dataset and then vectorize the data
  value_before <- sf |> 
    dplyr::filter(Year == StartYear) |> 
    dplyr::rename("PreValue" = dplyr::all_of(Col)) |> 
    dplyr::left_join(pre_value_df) |> 
    dplyr::rename(Code = PreCode) |> 
    terra::vect()
  
  #write the after codes to the dataset and then vectorize the data
  value_after <- sf |> 
    dplyr::filter(Year == EndYear) |> 
    dplyr::rename("PostValue" = dplyr::all_of(Col)) |> 
    dplyr::left_join(post_value_df) |> 
    dplyr::rename(Code = PostCode) |> 
    terra::vect()
  
  #create a grid of raster cells based of the spatvect (this determines raster resolution) (note they can use the same grid)
  grid <- terra::rast(value_before, nrow = Resolution, ncol = Resolution)
  
  #rasterize each dataset (Note that the variable "code" is what value is assigned to each cell)
  #then subtract one raster from another (each cell has its unique code, so the value of the subtraction will be linked to a unique output)
  change_in_value <- (terra::rasterize(value_before, grid, field = "Code") - terra::rasterize(value_after, grid, field = "Code"))
  
  #convert raster back to a polygon and then to an sf object and bind the landuse code/key table
  change_in_value <- sf::st_as_sf(terra::as.polygons(change_in_value)) |> 
    dplyr::left_join(value_change_key, by = c("Code" = "CodeChange"))

  #return the dataset
  return(change_in_value)
}  
