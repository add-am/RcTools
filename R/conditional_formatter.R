#' A Helper That Presents Conditional Formatting Options
#'
#' @param data The main dataframe to run functions on
#' @param data_og The original dataframe before "as.numeric" is applied per column.
#' @param workbook An excel workbook object.
#' @param target_columns Numeric Vector. The columns that colour grading should be applied to. e.g. 1:10.
#' @param target_rows Numeric Vector. The rows that colour grading should be applied to. e.g. 1:10.
#' @param scheme String. Defines the conditional formatting rules and colours. Options are Report Card Grade, Report Card Score, Rainfall Temperature, Summary Stat
#' @param file_name String. The name of the file you want to save. Without the extension
#'
#' @returns An excel workbook object
#'
#' @examples
#' \dontrun{
#' conditional_formatter(data, data_og, workbook, target_columns, target_rows, scheme, file_name)
#' }
conditional_formatter <- function(data, data_og, workbook, target_columns, target_rows, scheme, file_name){

  #convert target_columns and target_rows into excel format
  dimensions <- paste0(
    LETTERS[min(target_columns)],
    min(target_rows)+1, 
    ":",
    LETTERS[max(target_columns)], 
    max(target_rows)+1
  )

  #standardise inputs
  scheme <- stringr::str_to_lower(scheme)

  if (scheme == "report card grade"){#use text based rules to colour with Report card styling
    
    #create a list of rule conditions to iterate on
    match_conditions <- list(
      match_vals = c("(A)", "(B)", "(C)", "(D)", "(E)"),
      style = c("dark_green", "light_green", "yellow", "orange", "red")
    )

    #for each item in the list, create a formatting rule
    for (i in seq_along(match_conditions$match_vals)) {
      workbook$add_conditional_formatting(
        "Data",
        dims = dimensions,
        type = "containsText", 
        rule = match_conditions$match_vals[i], 
        style = match_conditions$style[i]
      )
    }

  } else if (scheme == "report card score") {#use numeric based rules to colour with Report card styling
    
    #create a list of min and max values to iterate on
    match_conditions <- list(
      min_vals = c(81, 61, 41, 21, 0),
      max_vals = c(100, 80.9, 60.9, 40.9, 20.9),
      style = c("dark_green", "light_green", "yellow", "orange", "red")
    )

    #for each item in the list, create a formatting rule
    for (i in seq_along(match_conditions$min_vals)) {
      workbook$add_conditional_formatting(
        "Data",
        dims = dimensions,
        type = "between", 
        rule = c(match_conditions$min_vals[i], match_conditions$max_vals[i]), 
        style = match_conditions$style[i]
      )
    }

    #replace everything that isnt supposed to be NA (i.e. character cells) with their original value
    for (column_number in seq_len(ncol(data_og))) {
      for (row_number in seq_len(nrow(data_og))) {
        if (is.na(data[row_number, column_number])) {
          workbook$add_data(
            "Data", 
            as.character(data_og[row_number, column_number]), 
            start_col = column_number, 
            target_rows = row_number + 1)
        }
      }
    }

  } else if (scheme == "rainfall") {

  } else if (scheme == "temperature") {

  } else if (scheme == "summary stat"){

  }

  openxlsx2::wb_save(workbook, file = paste0(file_name, ".xlsx"), overwrite = T)
  
}
