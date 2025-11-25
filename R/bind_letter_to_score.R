#create function that determines letter and binds it to the value
#' Helper to Extract A Letter Grade From Scores
#'
#' @param df A dataframe/table/spreadsheet
#' @param columns Range. The column indices to be targeted.
#' @param rows Range. The row indicies to be targeted.
#'
#' @returns A dataframe with letters attached to scores
#'
#' @importFrom rlang sym
#' @examples
#' \dontrun{
#' df <- data.frame(
#'  "Areas" = c("A", "B", "C", "D", "E", "F"),
#'  "Score1" = c(0, 20, 40, 60, 80, 100),
#'  "Score1" = c(0, 20, 40, 60, 80, 100),
#'  "Other" = c("Z", "Y", "X", "W", "V", "U")
#' )
#' column_indicies <- 2:3
#' row_indicies <- 1:4
#' bind_letter_to_score(df, column_indicies, row_indicies)
#' }
bind_letter_to_score <- function(df, columns, rows) {

  #create a new column that contains the original column index (for rebuilding)
  df <- df |> 
    dplyr::mutate(RowId = dplyr::row_number())
    
  #slice out the target rows
  df_slice <- df |> 
    dplyr::slice(min(rows):max(rows))

  #keep opposite
  reverse_slice <- df |> 
    dplyr::slice(-c(min(rows):max(rows)))
  
  #get the column names based on the column indices provided
  column_names <- colnames(df_slice[columns])     

  #create new columns with the letter grades based on the values in the target columns
  df_slice <- df_slice |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(column_names),
        ~ dplyr::case_when(
          .x >= 0 & .x < 21 ~ "(E)",
          .x >= 21 & .x < 41 ~ "(D)",
          .x >= 41 & .x < 61 ~ "(C)",
          .x >= 61 & .x < 81 ~ "(B)",
          .x >= 81 & .x < 101 ~ "(A)",
          TRUE ~ ""
        ),
        .names = "{.col}_grade"
      )
    )
  
  #for each of the columns, combine the score and grade together
  for (target_column in column_names){

    #build the grade column name
    grade_column_name <- paste0(target_column, "_grade")

    #unite the columns
    df_slice <- df_slice |> 
      tidyr::unite(
        !!sym(target_column), 
        c(target_column, grade_column_name), 
        sep = " ", 
        remove = TRUE
      )
  }

  #join the slice back with the rest, order by row id, remove row id
  df <- df_slice |> 
    rbind(reverse_slice) |> 
    dplyr::arrange(RowId) |> 
    dplyr::select(-RowId)

  return(df)
        
}

