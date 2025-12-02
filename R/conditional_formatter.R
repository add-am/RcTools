#' A Helper That Presents Conditional Formatting Options
#'
#' @param data The main dataframe to run functions on
#' @param workbook An excel workbook object.
#' @param target_columns Numeric Vector. The columns that colour grading should be applied to. e.g. 1:10.
#' @param target_rows Numeric Vector. The rows that colour grading should be applied to. e.g. 1:10.
#' @param scheme String. Defines the conditional formatting rules and colours. Options are Report Card Grade, Report Card Score, Rainfall Temperature, Summary Stat
#' @param include_letter Boolean. Should a letter grade be attached to the score? This is specific to Report Card scores
#' @param file_name String. The name of the file you want to save. Without the extension
#'
#' @returns An excel workbook object
#'
#' @examples
#' \dontrun{
#' conditional_formatter(
#' data, workbook, target_columns, 
#' target_rows, scheme, include_letter, file_name)
#' }
conditional_formatter <- function(
  data, workbook, target_columns, 
  target_rows, scheme, include_letter, file_name){

  #convert target_columns and target_rows into excel format
  dimensions <- paste0(
    LETTERS[min(target_columns)],
    min(target_rows)+1, 
    ":",
    LETTERS[max(target_columns)], 
    max(target_rows)+1
  )

  #convert scheme, if need, to include either grade or score
  if (scheme == "report_card"){
    if (include_letter){
      scheme <- "report_card_grade"
    } else {
      scheme <- "report_card_score"
    }
  }

  if (scheme == "report_card_grade"){#use text based rules to colour with Report card styling
    
    #create a list of rule conditions to iterate on
    match_conditions <- list(
      match_vals = c("A", "B", "C", "D", "E"),
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

  } else if (scheme == "report_card_score") {#use numeric based rules to colour with Report card styling
    
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

  } else if (scheme %in% c("rainfall", "temperature")) {#use numeric based rules to colour with Rainfall or Temperature styling

    #create a list of rule conditions to iterate on
    match_conditions <- list(
      match_vals = c("1", "2", "3", "4", "5", "6", "7"),
      style = c("lowest", "very_much_below", "below", "average", "above", "very_much_above", "highest")
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

  } else if (scheme == "summary_statistic"){

    #create a list of rule conditions to iterate on
    match_conditions <- list(
      match_vals = c(
        paste0(LETTERS[min(target_columns)], "2 <= ", LETTERS[max(target_columns)+1], "2"),
        paste0(LETTERS[min(target_columns)], "2 > ", LETTERS[max(target_columns)+1], "2")
      ),
      style = c("blue", "orange")
    )

    #for each item in the list, create a formatting rule
    for (i in seq_along(match_conditions$match_vals)) {
      workbook$add_conditional_formatting(
        "Data",
        dims = dimensions,
        type = "expression", 
        rule = match_conditions$match_vals[i], 
        style = match_conditions$style[i]
      )
    }

  } else if (scheme == "presence_absence") {#use numeric based rules to colour with Presence Absence styling

    #create a list of rule conditions to iterate on
    match_conditions <- list(
      match_vals = c("1", "0"),
      style = c("blue", "grey")
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

  }

  openxlsx2::wb_save(workbook, file = paste0(file_name, ".xlsx"), overwrite = T)
  
}
