#inputs: 
#supplied_sf_1 = REQUIRED. The main sf object that will define the area of interest
#supplied_sf_2 = OPTIONAL. If you want a second box in the map you can use this
#backdrop = REQUIRED. The "backdrop" aka wider region that provides context to where the main sf object is
#aspect = REQUIRED. The aspect ratio of the inset map (Height and Width). Note this should match that of the aspect used over in the main map

#' Create a Tmap Inset (corner) Map
#'
#' @param supplied_sf_1 Sf object. An sf object that defines the main area of interest.
#' @param background Sf object. An sf object that defines the greater region around the area of interest. 
#' For example if supplied_sf_1 is a collection of water quality samples, background would be the basin in which they were collected.
#' @param aspect Numeric String. A numeric value that describes the aspect ratio of the parent map. 
#' This ensures correct positioning of the inset map.
#' @param use_bbox_1 Boolean. Should a bounding box of the sf object be used (TRUE) or the actual outline of the sf object (FALSE). 
#' Deafults to TRUE.
#' @param supplied_sf_1_color Character String. A character value that changes the colour of the supplied_sf_1 object. Defaults to red.
#' @param background_color Character String. A character value that changes the colour of the background object. Defaults to white.
#' @param supplied_sf_2 Sf object. An sf object. An sf object that defines the secondary area of interest. Defaults to NULL
#' @param use_bbox_2 Boolean. Should a bounding box of the sf object be used (TRUE) or the actual outline of the sf object (FALSE). 
#' Deafults to TRUE.
#' @param supplied_sf_2_color Character String. A character value that changes the colour of the supplied_sf_2 object. Defaults to blue.
#'
#' @returns
#'
#' @export
#' @examples
maps_inset_layer <- function(supplied_sf_1, background, aspect, use_bbox_1 = TRUE, supplied_sf_1_colour = "red", 
  background_color = "white", supplied_sf_2 = NULL, use_bbox_2 = TRUE, supplied_sf_2_colour = "blue"){
  
  #check required arguments
  if (any(missing(supplied_sf_1), missing(background), missing(aspect))){stop("You must supply at least the 'supplied_sf_1', 'background' and 'aspect' parameters.")}

  #check required arguement types
  if (!inherits(supplied_sf_1, "sf")){stop("The object supplied to the 'supplied_sf_1' parameter must be of type sf.")}
  if (!inherits(background, "sf")){stop("The object supplied to the 'background' parameter must be of type sf.")}
  if (!is.numeric(aspect)){stop("The object supplied to the 'aspect' parameter must be of numeric type.")}
  
  #check other arguements
  if (!is.logical(use_bbox_1)){stop("You must supply a boolean arguement to the 'use_bbox_1' parameter.")}
  if (!is.character(supplied_sf_1_color)){stop("The object supplied to the 'supplied_sf_1_color' parameter must be of character type.")}
  if (!is.character(background_color)){stop("The object supplied to the 'background_color' parameter must be of character type.")}
  if (!is.null(supplied_sf_2) & !inherits(supplied_sf_2, "sf")){stop("The object supplied to the 'supplied_sf_2' parameter must be of type sf.")}
  if (!is.logical(use_bbox_2)){stop("You must supply a boolean arguement to the 'use_bbox_2' parameter.")}
  if (!is.character(supplied_sf_2_color)){stop("The object supplied to the 'supplied_sf_2_color' parameter must be of character type.")}
  
  #if the user wants bounding boxes, convert the objects
  if (use_bbox_1){sf_object_1 <- sf::st_as_sfc(st_bbox(supplied_sf_1))}
  if (use_bbox_2 & !is.null(supplied_sf_2)){sf_object_2 <- sf::st_as_sfc(st_bbox(supplied_sf_2))}
 
  #otherwise, just reduce to the outline of the object (st_union)
  if (!use_bbox_1){sf_object_1 <- sf::st_union(supplied_sf_1)}
  if (!use_bbox_2 & !is.null(supplied_sf_2)){sf_object_2 <- sf::st_union(supplied_sf_2)}

  #load in the qld outline
  load("inst/extdata/qld.RData")

  #create the map that will be put into the viewport
  inset_map <-
    tmap::tm_shape(qld) +
    tmap::tm_polygons(fill = "grey80", col = "black") +
    tmap::tm_shape(background, is.main = TRUE) +
    tmap::tm_polygons(fill = "grey90", col = "black") +
    tmap::tm_shape(sf_object_1) +
    tmap::tm_borders(lwd = 2, col = supplied_sf_1_color) +
    tmap::tm_layout(asp = aspect, bg.color = background_color, outer.bg.color = background_color) 
  
  #if the user requested a second object, build it
  if (!is.null(supplied_sf_2)){
    
    inset_map <- 
      inset_map + 
      tmap::tm_shape(sf_object_2) +
      tmap::tm_borders(lwd = 2, col = supplied_sf_2_color, zindex = 999)
  }
  
  #figure out where to place the view port
  inset_viewport <- grid::viewport(x = 0.97, y = 0.97, width = 0.2, height = 0.2, just = c("right", "top"))
  
  #combine the map, and its positioning into a list
  func_output <- list("Inset Map" = inset_map, "Inset Map Positioning" = inset_viewport) 
  
  #return the list of items
  return(func_output)

}
