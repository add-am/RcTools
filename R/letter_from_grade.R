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
#' letters_on_grade(df, column_index)
#' 
letters_on_grade <-  function(df, columns) {
  
  #get the column names based on the column indices provided
  column_names <- colnames(df[columns])     

  #create new columns with the letter grades based on the values in the target columns
  df <- df |> 
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
    df <- df |> 
      tidyr::unite(
        !!sym(target_column), 
        c(target_column, grade_column_name), 
        sep = " ", 
        remove = TRUE
      )
  }

  return(df)
        
}

