cond_form_rc_grades <- function(df, file_name, col, row, method){
  
  #make lowercase
  method <- stringr::str_to_lower(method)
  
  #create a duplicate that doesn't get all columns converted to numeric
  df_original <- df

  #coerce cols to numeric - cols may not be numeric if they contain "weird" Nan, NA, or ND values
  df[col] <- purrr::map(df[col], ~suppressWarnings(as.numeric(.)))

  #convert cols and rows into excel format
  dimensions <- paste0(LETTERS[min(col)], min(row), ":", LETTERS[max(col)], max(row))
 
  #create an empty workbook
  wb <- openxlsx2::wb_workbook()
  
  #put the data into the workbook
  wb$add_worksheet("Data")
  wb$add_data("Data", df)

  #create a list of colours to add to the workbook
  col_vect <- list(
    fnt_col = c("#000000", "#000000", "#000000", "#000000", "#000000"),
    bkg_col = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF0000"),
    col_nm = c("dark_green", "light_green", "yellow", "orange", "red")
  )

  #for each item in the list, add it to the workbook
  for (i in seq_along(col_vect$fnt_col)) {
    wb$styles_mgr$add(
      openxlsx2::create_dxfs_style(
        font_color = openxlsx2::wb_colour(col_vect$fnt_col[i]),
        bgFill = openxlsx2::wb_colour(col_vect$bkg_col[i])
      ),
      col_vect$col_nm[i]
    )
  }
    
  if (method == "numeric"){
      
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
            wb$add_data("Data", as.character(df_original[rn,cn]), start_col = cn, start_row = rn+1)
          }
        }
      }
        
      #save the workbook
      openxlsx2::wb_save(wb, file = paste0(file_name, ".xlsx"), overwrite = T)
        
    }
      
    #run formatting on df
    cond_format_num(df_in = df, output = file_name)
      
  } else if (method == "letter"){ 
      
    #create function that determines letter and binds it to the value
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
      
    #create a counter that starts at the first col designated by the user input
    x <- col[1]
    
    #determine the index of the first new column that will be created by the loop
    y <- ncol(df) + 1
      
    #run the letter function the same number of times as there is columns to target, joining the 
    # outputted letter col back to the inputted score col each loop
    for (i in 1:length(col)){

      #run the letter function on the targeted column
      df <- letters_on_grade(df, x)

      #get the name for the column that was input
      col_name <- as.character(colnames(df[x]))
        
      #coerce inputted score col to character and then join the number and letter
      df <- df |> 
        dplyr::mutate({{col_name}} := as.character(.data[[col_name]])) |>
        tidyr::unite({{col_name}}, x, y, sep = " ", remove = T)

        
      #increase the column counter so that the next col is inputted before starting again
      x = x + 1
    }
      
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
      
  }

} 