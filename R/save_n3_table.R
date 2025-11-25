#' Conditional Formatting with .xlsx Output
#'
#' @param df A standard dataframe, data.frame, table, spreadsheet, etc.
#' @param file_name String. The name to save the output under, without an extension.
#' @param target_columns Numeric Vector. The columns that colour grading should be applied to. e.g. 1:10.
#' @param target_rows Numeric Vector. The rows that colour grading should be applied to. e.g. 1:10.
#' @param scheme String. One of several options: Report Card, Rainfall, Temperature, Summary Statistic, Presence Absence. 
#' This determines the colour scheme and conditional formatting schemes used.
#' @param include_letter Boolean. Should a letter grade be attached to the score? This is specific to Report Card scores
#' (under the Report Card scheme).
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
#' save_n3_table(
#'  df = x, 
#'  file_name = "final_scores", 
#'  target_columns = 2:3, 
#'  target_rows = 2:5, 
#'  scheme = "Report Card",
#'  include_letter = FALSE
#' )
#' }
save_n3_table <- function(df, file_name, target_columns, target_rows, scheme, include_letter = FALSE){

  #standardise inputs
  scheme <- scheme |> 
    stringr::str_to_lower() |> 
    stringr::str_replace_all(" |\\.|-", "_")

  #create a duplicate that doesn't get all columns converted to numeric (used within conditional formatting func)
  df_original <- df

  #coerce all cols to numeric - cols may not be numeric if they contain "weird" Nan, NA, or ND values
  df[target_columns] <- purrr::map(df[target_columns], ~suppressWarnings(as.numeric(.)))

  #create an empty workbook
  wb <- openxlsx2::wb_workbook()
  
  #put the data into the workbook
  wb$add_worksheet("Data")
  wb$add_data("Data", df)

  #make a list of colours in the workbook using the colour scheme helper
  wb <- define_colour_scheme(workbook = wb, scheme = scheme)
     
  if (include_letter){ 

    #run the letter from grade function
    df <- bind_letter_to_score(df, target_columns, target_rows)
      
    #put the new data to the workbook, over riding the old data that was added
    wb$add_data("Data", df)

  }
    
  #run the conditional formatting function. This also saves the data
  conditional_formatter(df, df_original, wb, target_columns, target_rows, scheme, include_letter, file_name)
    
}