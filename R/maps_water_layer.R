#' Create a Tmap Water Layer
#'
#' @param Basin Character Vector. Defines the area in which the water layer should be created.
#' @param WaterLines Boolean. Should water lines be included in the map layer? Defaults to TRUE.
#' @param WaterAreas Boolean. Should water areas be included in the map layer? Defaults to TRUE.
#' @param WaterLakes Boolean. Should lakes be included in the map layer? Defaults to TRUE.
#' @param StreamOrder Numeric Vector of length 1 or 2. Defines the minimum, or minimum and maximum, stream order to filter the WaterLines 
#' paramter by. Where 1 = the smallest streams and increasing numbers correspond to increasing stream sizes. Defaults to NULL (all).
#'
#' @returns A Tmap Object
#'
#' @export
#' @examples
#' 
#' ross_and_black <- maps_water_layer(Basin = c("Black", "Ross"))
#' 
maps_water_layer <- function(Basin, WaterLines = TRUE, WaterAreas = FALSE, WaterLakes = FALSE, StreamOrder = NULL){

  #majority of function is simply passing objects to the extract_watercourses() function
  all_water_data <- extract_watercourses(Basin, WaterLines, WaterAreas, WaterLakes, StreamOrder)

  #get a list of geometry types in the data
  geom_types <- unique(sf::st_geometry_type(all_water_data))

  #start with an empty tmap object 
  water_layer <- tmap::tm_shape(all_water_data)

  #if the data has lines :
  if (any(c("MULTILINESTRING", "LINESTRING") %in% geom_types)){

    #build lines layer
    water_layer <- water_layer +
      tmap::tm_lines(col = "dodgerblue", lwd = 0.5)

  } 

  #if the data has areas
  if (any(c("POLYGON", "MULTIPOLYGON") %in% geom_types)){
    
    #build area layer
    water_layer <- water_layer +
      tmap::tm_polygons(fill = "aliceblue", col = "dodgerblue", lwd = 0.5)
  
  }
  
  #return the water layer object
  return(water_layer)
  
}

