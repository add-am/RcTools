#' Split a Table into N rows
#'
#' @param df A single dataframe or a list of dataframes
#' @param RowCount The number of rows per split dataframe. Defaults to 1500
#' @param NameVec A single dataframe name or list of dataframe names, of equal length to the number of dataframes provided under df.
#' Defaults to NULL and will fill dataframe names with the generic "table_n"
#'
#' @returns A list of named dataframes
#'
#' @export
#' @examples
#' 
#' #a simple example with a single dataframe
#' df <- data.frame(Name = rep("example_name", 10000), Value = rep(1, 10000))
#' split_dfs <- row_splitter(df, 1000, "my_df")
#' 
#' #an example with several dataframes
#' a_2025 <- data.frame(Name = rep("A", 10000), Year = rep(2025, 10000), Value = rep(1, 10000))
#' a_2024 <- data.frame(Name = rep("A", 10000), Year = rep(2024, 10000), Value = rep(1, 10000))
#' b_2025 <- data.frame(Name = rep("B", 10000), Year = rep(2025, 10000), Value = rep(1, 10000))
#' b_2024 <- data.frame(Name = rep("B", 10000), Year = rep(2024, 10000), Value = rep(1, 10000))
#' 
#' df_list <- list(a_2025, a_2024, b_2025, b_2024)
#' df_names <- list("a_2025"," a_2024", "b_2025", "b_2024")
#' 
#' split_dfs <- row_splitter(df_list, 1000, df_names)
#' 
row_splitter <- function(df, RowCount = 1500, NameVec = NULL){

  #check required argument
  if (missing(df)){stop("You must supply at least the 'df' parameter.")}

  #check type of df argument
  if(!(inherits(df, "data.frame") | inherits(df, "list"))){
    stop("You must supply either a data.frame or list object to the 'df' parameter.")
  }

  #check argument types
  if (!is.numeric(RowCount)){stop("You must supply a numeric argument to the 'RowCount' parameter")}
  if (!(is.character(NameVec) | is.list(NameVec))){
    stop("You must supply a character vector or character list object to the 'NameVec' parameter")
  }

  #if a single df is provided, convert it to a list so it runs through purr smoothly
  if (inherits(df, "data.frame")){df <- list(df)}

  #map over the list of datasets
  list_of_dfs <- purrr::map2(df, seq_along(df), function(x, count){
    
    #build a vector of min and max row indicies
    min_indicies <- seq(1, nrow(x), RowCount)
    max_indicies <- pmin(min_indicies + (RowCount-1), nrow(x))

    if (!is.null(NameVec)){table_names <- paste0(NameVec[count], "_rows_", min_indicies, "_to_", max_indicies)}
    if (is.null(NameVec)){table_names <- paste0("table_", count, "_rows_", min_indicies, "_to_", max_indicies)}

    #slice up the dataframe into small chunks and return it as a list
    small_tables <- purrr::map2(min_indicies, max_indicies, ~dplyr::slice(x, .x:.y))

    #name each of the mini dataframes in the list
    names(small_tables) <- table_names

    #return the list of dataframes
    small_tables
    
  })

  #flatten the list of lists
  list_of_dfs <- unlist(list_of_dfs, recursive = FALSE)
  
}


