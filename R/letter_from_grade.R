#create function that determines letter and binds it to the value
#' Helper to Extract A Letter Grade From Scores
#'
#' @param df_in
#' @param column_target
#'
#' @returns
#'
#' @export
#' @examples
letters_on_grade <-  function(df_in, column_target) {
  
  #get the column name based on the column index
  col_name <- colnames(df_in[column_target])     

  #create a new column with the letter grade based on the value in the target column
  df_in |> 
    dplyr::mutate("{col_name}_grade" := dplyr::case_when(
      df_in[column_target] >= 0 & df_in[column_target] < 21 ~ "(E)",
      df_in[column_target] >= 21 & df_in[column_target] < 41 ~ "(D)",
      df_in[column_target] >= 41 & df_in[column_target] < 61 ~ "(C)",
      df_in[column_target] >= 61 & df_in[column_target] < 81 ~ "(B)",
      df_in[column_target] >= 81 & df_in[column_target] < 101 ~ "(A)",
      TRUE ~ "")
    )
        
}