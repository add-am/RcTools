#create function that determines letter and binds it to the value
#' Helper to Extract A Letter Grade From Scores
#'
#' @param df A dataframe/table/spreadsheet
#' @param columns Range. The column indices to be targeted.
#'
#' @returns A dataframe
#'
#' @examples
#' column_index <- 1:10
#' letters_from score(df, column_index)
#' 
letters_from_score <-  function(df, columns, rows) {
  
  #create a new column that contains the original column index (for rebuilding)
  df <- df |> 
    dplyr::mutate(RowId = dplyr::row_number())
    
  #slice out the target rows
  df_slice <- df |> 
    dplyr::slice(min(rows): max(rows))

  #keep opposite
  reverse_slice <- df |> 
    dplyr::slice(-c(min(rows): max(rows)))
  
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

