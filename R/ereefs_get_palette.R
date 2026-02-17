#' Return Colour Palettes Associated With Specific Variables
#'
#' @param Variable Character String. The Variable being mapped, currently accessible: "Turbidity", "Chlorophyll a", "DIN", "NH4", "NO3", "Secchi", "pH", "Wind", "True Colour"
#'
#' @returns A vector of hex code colour values of length 9
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' ereefs_get_palette("Turbidity")
#' }
#' 
ereefs_get_palette <- function(Variable){

  #check variables by comparing to allowed variables (at the moment)
  var_choices <- c("Turbidity", "Chl_a_sum", "DIN", "NH4", "NO3", "Secchi", "PH", "Wind", "True Colour")

  stringr::str_detect(Variable, var_choices)
  if (!any(stringr::str_detect(Variable, var_choices))){
    stop("Invalid 'Variable': must be one of ", paste(var_choices, collapse = ", "))
  }

  #defined the match that occured
  matched_name <- var_choices[stringr::str_detect(Variable, var_choices)]
  
  #define the list of colours to choose from
  colour_key <- list( 
    "Turbidity"      = oce::oceColorsTurbidity(9),
    "Chl_a_sum"      = oce::oceColorsChlorophyll(9),
    "DIN"            = RColorBrewer::brewer.pal(9, "Blues"),
    "NH4"            = rcartocolor::carto_pal(9, "ag_GrnYl"),
    "NO3"            = rcartocolor::carto_pal(9, "Sunset"),
    "Secchi"         = RColorBrewer::brewer.pal(9, "OrRd"),
    "PH"             = RColorBrewer::brewer.pal(9, "RdBu"),
    "Wind"           = oce::oceColorsVelocity(9)
  )

  #do any of the names match the requested variable? returns a T/F vector
  matches <- stringr::str_detect(Variable,  names(colour_key))

  #return associated palette
  return(colour_key[matches][[1]])
  
}