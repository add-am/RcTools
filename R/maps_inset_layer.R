#inputs: 
#supplied_sf_1 = REQUIRED. The main sf object that will define the area of interest
#supplied_sf_2 = OPTIONAL. If you want a second box in the map you can use this
#backdrop = REQUIRED. The "backdrop" aka wider region that provides context to where the main sf object is
#aspect = REQUIRED. The aspect ratio of the inset map (Height and Width). Note this should match that of the aspect used over in the main map

maps_inset_layer <- function(supplied_sf_1, use_bbox_1 = TRUE, supplied_sf_2 = NA, use_bbox_2 = TRUE,
                             background, background_color = "white", aspect){
  
  #create a vector of package dependencies
  package_vec <- c("tmap", "grid", "tidyverse", "sf")

  #apply the function "require" to the vector of packages to install and load dependencies
  lapply(package_vec, require, character.only = T)

  #if the user wants bounding boxes for the supplied sf object(s):
  if (use_bbox_1){
  
    #create a bounding box of the first supplied_sf then convert it to a sfc (simple feature collection) object
    sf_object_1 <- st_as_sfc(st_bbox(supplied_sf_1))
    
  }
  
  #if there is another sf, and they want it as a bounding box, then create a bbox for that as well
  if(!all(is.na(supplied_sf_2)) & use_bbox_2){
      
    #create a bounding box of the second supplied_sf then convert it to a sfc (simple feature collection) object
    sf_object_2 <- st_as_sfc(st_bbox(supplied_sf_2))
  
  }
  
  #otherwise, if the use doesn't want the objects changed to bboxes then just reduce to the outline (st_union)
  if (!use_bbox_1){
    
    #union the object to extract just the outline
    sf_object_1 <- st_union(supplied_sf_1)
    
  }
  
  #if there is another sf, then extract the outline for that as well
  if(!all(is.na(supplied_sf_2)) & !use_bbox_2){
      
    #union the object to extract just the outline
    sf_object_2 <- st_union(supplied_sf_2)
      
  }
    
  #create the map that will be put into the viewport
  inset_map <- tm_shape(background, is.main = T) +
    tm_polygons() +
    tm_shape(qld) +
    tm_polygons(fill = "grey80", col = "black") +
    tm_shape(background) +
    tm_polygons(fill = "grey90", col = "black") +
    tm_shape(sf_object_1) +
    tm_borders(lwd = 2, col = "red") +
    tm_layout(asp = aspect,
              bg.color = "white",
              outer.bg.color = background_color) 
  
  if (!all(is.na(supplied_sf_2))){
    
    inset_map <- inset_map + 
      tm_shape(sf_object_2) +
      tm_borders(lwd = 2, col = "blue", zindex = 999)
  }
  
  #figure out where to place the view port
  inset_viewport <- viewport(x = 0.97, y = 0.97, width = 0.2, height = 0.2, just = c("right", "top"))
  
  #assign the inset map and view port to the global environment
  assign("inset_map", inset_map, envir = globalenv())
  assign("inset_viewport", inset_viewport, envir = globalenv())
    
  message("\nThe variables 'inset_map' and 'inset_viewport' were assigned to the global environment to use for an inset map.\n")

}
