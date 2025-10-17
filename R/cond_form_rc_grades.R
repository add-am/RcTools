#' Conditional Formatting with .xlsx Output
#'
#' @param df A standard dataframe, data.frame, table, etc.
#' @param file_name String. The name to save the output under, without an extension.
#' @param target_columns Numeric Vector. The columns that colour grading should be applied to. e.g. 1:10.
#' @param target_rows Numeric Vector. The rows that colour grading should be applied to. e.g. 1:10.
#' @param include_letter Boolean. Should a letter grade be attached to the score?
#'
#' @returns A .xlsx output
#'
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(
#'  Basin = c("Black", "Ross", "Haughton", "Suttor"),
#'  DIN = c(51, 76, 27, 98),
#'  TP = c(90, 57, 34, 72)
#' )
#' cond_form_rc_grades(
#'  df = x, 
#'  file_name = "final_scores", 
#'  target_columns = 2:3, 
#'  target_rows = 2:5, 
#'  include_letter = FALSE
#' )
#' }
cond_form_rc_grades <- function(df, file_name, target_columns, target_rows, include_letter){
    
  #create a duplicate that doesn't get all columns converted to numeric
  df_original <- df

  #coerce all cols to numeric - cols may not be numeric if they contain "weird" Nan, NA, or ND values
  df[target_columns] <- purrr::map(df[target_columns], ~suppressWarnings(as.numeric(.)))

  #convert target_columns and target_rows into excel format
  dimensions <- paste0(LETTERS[min(target_columns)], min(target_rows)+1, ":", LETTERS[max(target_columns)], max(target_rows)+1)
 
  #create an empty workbook
  wb <- openxlsx2::wb_workbook()
  
  #put the data into the workbook
  wb$add_worksheet("Data")
  wb$add_data("Data", df)

  #add a list of colours to the workbook using the colour scheme helper
  wb <- define_colour_scheme(wb, "Report Card")
    
  if (include_letter){ 
      
    #create function that determines letter and binds it to the value
    #letters_on_grade <-  function(df_in, column_target) {

      #get the column name based on the column index
    #  col_name <- colnames(df_in[column_target])     

      #create a new column with the letter grade based on the value in the target column
    #  df_in |> 
    #    dplyr::mutate("{col_name}_grade" := dplyr::case_when(
    #      df_in[column_target] >= 0 & df_in[column_target] < 21 ~ "(E)",
    #      df_in[column_target] >= 21 & df_in[column_target] < 41 ~ "(D)",
    #      df_in[column_target] >= 41 & df_in[column_target] < 61 ~ "(C)",
    #      df_in[column_target] >= 61 & df_in[column_target] < 81 ~ "(B)",
    #      df_in[column_target] >= 81 & df_in[column_target] < 101 ~ "(A)",
    #      TRUE ~ "")
    #    )
        
    #}
      
    #include row index
    df <- df |> 
      dplyr::mutate(RowId = dplyr::row_number())
    
    #slice out the target rows
    df_slice <- df |> 
      dplyr::slice(min(target_rows): max(target_rows))

    #keep opposite
    reverse_slice <- df |> 
      dplyr::slice(-c(min(target_rows): max(target_rows)))

    #create a counter that starts at the first col designated by the user input
    #x <- target_columns[1]
    
    #determine the index of the first new column that will be created by the loop
    #y <- ncol(df_slice) + 1

    #run the letter function the same number of times as there is columns to target, joining the 
    # outputted letter col back to the inputted score col each loop
    #for (i in 1:length(target_columns)){

      #run the letter function on the targeted column
    #  df_slice <- letters_on_grade(df_slice, x)

      #get the name for the column that was input
    #  col_name <- as.character(colnames(df_slice[x]))
        
      #coerce inputted score col to character and then join the number and letter
    #  df_slice <- df_slice |> 
    #    dplyr::mutate({{col_name}} := as.character(.data[[col_name]])) |>
    #    tidyr::unite({{col_name}}, x, y, sep = " ", remove = T)

        
      #increase the column counter so that the next col is inputted before starting again
    #  x = x + 1
    #}

    df_slice <- letters_on_grade(df_slice, target_columns)

    #join the slice back with the rest, order by row id, remove row id
    df <- df_slice |> 
      rbind(reverse_slice) |> 
      dplyr::arrange(RowId) |> 
      dplyr::select(-RowId)
      
    #add the new data to the workbook, over riding the old data that was added
    wb$add_data("Data", df)
      
    #formatting for letter grades
    cond_format_let <- function(df_in, output) {

      #create a list of rule conditions to iterate on
      match_conditions <- list(
        match_vals = c("(A)", "(B)", "(C)", "(D)", "(E)"),
        style = c("dark_green", "light_green", "yellow", "orange", "red")
      )

      #for each item in the list, create a formatting rule
      for (i in seq_along(match_conditions$match_vals)) {
        wb$add_conditional_formatting(
          "Data",
          dims = dimensions,
          type = "containsText", 
          rule = match_conditions$match_vals[i], 
          style = match_conditions$style[i]
        )
      }

      #save the workbook
      openxlsx2::wb_save(wb, file = paste0(output,".xlsx"), overwrite = T)
    }
      
    #run letter conditional formatting function on the data
    cond_format_let(df_in = df, output = file_name)
      
  } else {
      
    #create formatting function for numeric values from 0-100
    cond_format_num <- function(df_in, output) {

      #create a list of min and max values to iterate on
      min_max_vals <- list(
        min_vals = c(81, 61, 41, 21, 0),
        max_vals = c(100, 80.9, 60.9, 40.9, 20.9),
        style = c("dark_green", "light_green", "yellow", "orange", "red")
      )

      #for each item in the list, create a formatting rule
      for (i in seq_along(min_max_vals$min_vals)) {
        wb$add_conditional_formatting(
          "Data",
          dims = dimensions,
          type = "between", 
          rule = c(min_max_vals$min_vals[i], min_max_vals$max_vals[i]), 
          style = min_max_vals$style[i]
        )
      }
      
      #replace everything that isnt supposed to be NA (i.e. character cells) with their original value
      for (cn in seq_len(ncol(df_original))) {
        for (rn in seq_len(nrow(df_original))) {
          if (is.na(df[rn,cn])) {
            wb$add_data("Data", as.character(df_original[rn,cn]), start_col = cn, target_rows = rn+1)
          }
        }
      }
        
      #save the workbook
      openxlsx2::wb_save(wb, file = paste0(file_name, ".xlsx"), overwrite = T)
        
    }
      
    #run formatting on df
    cond_format_num(df_in = df, output = file_name)
  }
}