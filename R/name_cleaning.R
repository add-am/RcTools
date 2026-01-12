#inputs: 
#df <- every single df that is read should be passed through this function

#' A Name Cleaning Function for N3 Tables
#'
#' @param df Adf object of any dimensions. With or without a geometry column
#'
#' @returns A df object of the same dimensions as input
#'
#' @export
#' @examples
name_cleaning <- function(df){
  
  #check if the df is an sf object and if so, apply clean names to every column but the last column
  if(inherits(df, "sf")){
    
    #convert all but the geometry column to upper camel type
    df_new <- df |> 
      sf::st_drop_geometry() |>
      janitor::clean_names(case = "upper_camel")
   
    #extract the geometry column as it own object
    extract_geom_col <- sf::st_geometry(df)
    
    #bind the column back on with its new name. Note that it should also be named "geom"
    df_new <- df_new |>
      dplyr::mutate(#geom = sf::st_geometry(df), 
                    geom = sf::st_geometry(df)) |> 
      sf::st_as_sf()
  
  } else {
    
    #convert ALL columns to upper camel type, don't have to worry about geometry
    df_new <- df |> 
      janitor::clean_names(case = "upper_camel")

  }
  
  #for every character type column, run a encoding check and fix, then remove weird new line characters
  df_new <- df_new  |> 
    dplyr::mutate(across(where(is.character), ~ iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT'))) |> 
    dplyr::mutate(across(where(is.character), ~ stringr::str_replace_all(., "\r\n", " ")))
  
  return(df_new)
  
}
