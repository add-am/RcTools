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
maps_water_layer <- function(Basin, WaterLines = TRUE, WaterAreas = FALSE, WaterLakes = FALSE, StreamOrder = NULL){

  #check the required argument
  if (missing(Basin)){stop("You must supply at least the 'Basin' parameter.")}

  #check other arguments
  if (!is.logical(WaterLines)){stop("You must supply a boolean (T/F) argument to the 'WaterLines' parameter.")}
  if (!is.logical(WaterAreas)){stop("You must supply a boolean (T/F) argument to the 'WaterAreas' parameter.")}
  if (!is.logical(WaterLakes)){stop("You must supply a boolean (T/F) argument to the 'WaterLakes' parameter.")}
  if (!is.numeric(StreamOrder)){stop("You must supply a numeric argument to the 'StreamOrder' parameter.")}
  if (length(StreamOrder) > 2){stop("You must supply either one or two numeric arguments to the 'StreamOrder' parameter.")}

  #construct a list of allowed basins to request
  allowed_basins <- c(
    "Brisbane", "Burdekin", "Balonne-Condamine", "Coleman", "Barron", "Mary", "Burnett", "Holroyd", "Noosa", "Don", "Border Rivers",
    "Pine", "Ross", "Murray", "Archer", "Paroo", "Logan-Albert", "Wenlock", "Mitchell" , "Cooper Creek", "Normanby", "Haughton",
    "South Coast", "Diamantina", "Fitzroy", "Tully", "Herbert", "Mulgrave-Russell", "Burrum", "Bulloo", "Whitsunday Island", "Proserpine",
    "Georgina", "Endeavour", "Embley", "Lockhart", "Black",  "Maroochy", "Kolan", "Boyne", "Waterpark", "Calliope", "Gilbert", "Baffle",
    "Staaten", "Johnstone", "Moonie", "Settlement", "Mornington Island", "Fraser Island", "Curtis Island", "Flinders", "Jeannie", 
    "Olive-Pascoe", "Ducie", "Daintree", "Mossman", "Arafura Sea", "Jardine", "Nicholson", "Warrego", "Morning", "Leichhardt", "Norman",
    "Watson", "O'Connell", "Jacky Jacky", "Pioneer", "Coral Sea", "Stewart", "Moreton Bay Islands", "Plane", "Hinchinbrook Island", 
    "Shoalwater", "Styx", "Torres Strait Islands"
  )

  #check if the requested basin is in the allowed list
  if (!all(Basin %in% allowed_basins)){

    #collapse options into a single string
    allowed_basins_collapsed <- paste(allowed_basins, collapse = ", ")

    #determine which basin/basins failed
    faulty_basin <- paste(Basin[!Basin %in% allowed_basins], collapse = ", ")
    
    stop(glue::glue("The following basin argument(s) are invalid: '{faulty_basin}'. Try one of: {allowed_basins_collapsed}"))
  }

  #build the basin filter request (takes the format of an SQL argument)
  requested_basins <- paste("'", Basin,"'", collapse = ", ", sep = "")
  basin_query <- glue::glue("DRAINAGE_BASIN IN ({requested_basins})")

  #if stream order has be supplied, build this query as well
  if (!is.null(StreamOrder)){
    if (length(StreamOrder) == 1){
      stream_query <- glue::glue("STREAM_ORDER >= {StreamOrder}")
    } else if (length(StreamOrder == 2)){
      stream_query <- glue::glue("STREAM_ORDER >= {StreamOrder[1]} AND STREAM_ORDER <= {StreamOrder[2]}")
    }
  } 

  #build each element in the request list
  if (WaterLakes) {wc_lakes <- as.list(c("/27", "/28"))} else {wc_lakes <- NULL}
  if (WaterLines) {wc_lines <- list("/33")} else {wc_lines <- NULL}
  if (WaterAreas) {wc_areas <- list("/34")} else {wc_areas <- NULL}
  
  #combine elements into list, use compact to remove NULL items
  feature_list <- purrr::compact(c(wc_lakes, wc_lines, wc_areas))

  #fetch data
  requested_features <- purrr::map(feature_list, \(x) {

    #build the layer_url
    layer_url <- glue::glue("https://spatial-gis.information.qld.gov.au/arcgis/rest/services/InlandWaters/WaterCoursesAndBodies/MapServer{x}")
  
    #build the final filter query, based on the layer requested (if 33 it has a stream order argument)
    if (x == "/33"){
      final_query = glue::glue("{basin_query} AND {stream_query}")
    } else {
      final_query = basin_query
    }

    #open the layer
    layer <- arc_open(layer_url)
  
    #filter the layer based on user selection
    water_data <- arc_select(
      layer,
      where = final_query
  )
    
  #strip all information except for the geometry and an ID column
  water_data <- water_data |> 
    dplyr::mutate(ID = x) |> 
    dplyr::select(ID)

  return(water_data)

  })

  #bind all data together (makes the map easier to handle)
  all_water_data <- dplyr::bind_rows(requested_features)

  #build water layer
  water_layer <- 
    tmap::tm_shape(all_water_data) + #all lines
    tmap::tm_lines(col = "dodgerblue", lwd = 0.5) +
    tmap::tm_shape(all_water_data) + #all areas
    tmap::tm_polygons(fill = "aliceblue", col = "dodgerblue", lwd = 0.5)
  
  #return the water layer object
  return(water_layer)
  
}

