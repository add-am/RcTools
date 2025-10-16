#inputs: 
#df <- any tbl or data.frame
#file name <- whatever you want the output file to be named
#cols <- the index number of each column that the function should be applied to
#method <- either "numeric" or "letter". numeric only returns numbers, letter will return numbers and the associated letter grade.

cond_form_rc_grades <- function(df, file_name, cols, method = "numeric"){
  
  #----- Data frame preparation
  
  #open core libraries
  pacman::p_load(openxlsx2)
  
  #make lowercase and clean up the indicator variable
  method <- str_to_lower(method)
  
  #create a duplicate that doesn't get all columns converted to numeric
  df_original <- df

  #coerce cols to numeric - cols may not be numeric if they contain "weird" Nan, NA, or ND values
  df[cols] <- lapply(df[cols], function(x){suppressWarnings(as.numeric(x))})
  
  #----- Excel workbook preparation
  
  #create an empty workbook
  wb <- wb_workbook()
  
  #add the data to the workbook
  wb$add_worksheet("Data")
  wb$add_data("Data", df)
  
  #create the cell style we want
  dgn <- create_dxfs_style(font_color = wb_colour("#000000"), bgFill = wb_colour("#00B050")) #dark green
  lgn <- create_dxfs_style(font_color = wb_colour("#000000"), bgFill = wb_colour("#92D050")) #light green
  ylw <- create_dxfs_style(font_color = wb_colour("#000000"), bgFill = wb_colour("#FFFF00")) #yellow
  orn <- create_dxfs_style(font_color = wb_colour("#000000"), bgFill = wb_colour("#FFC000")) #orange
  red <- create_dxfs_style(font_color = wb_colour("#000000"), bgFill = wb_colour("#FF0000")) #red
  gry <- create_dxfs_style(font_color = wb_colour("#000000"), bgFill = wb_colour("#D9D9D9")) #grey
  neu <- create_dxfs_style(font_color = wb_colour("#000000"), bgFill = wb_colour("#FFFFFF")) #neutral (white)
  
  #put the style into the styles manager of the workbook
  wb$styles_mgr$add(dgn, "dark_green")
  wb$styles_mgr$add(lgn, "light_green")
  wb$styles_mgr$add(ylw, "yellow")
  wb$styles_mgr$add(orn, "orange")
  wb$styles_mgr$add(red, "red")
  wb$styles_mgr$add(gry, "grey")
  wb$styles_mgr$add(neu, "neutral")
  
  #----- Method 1: return only original cell values with colored backgrounds
    
  if (method == "numeric"){
      
    #create formatting function for numeric values from 0-100
    cond_format_num <- function(df_in, output) {
      
      wb$add_conditional_formatting("Data",
                                    cols = cols, rows = 1:nrow(df_in)+1,
                                    type = "between", rule = c(81, 100), style = "dark_green")
      wb$add_conditional_formatting("Data",
                                    cols = cols, rows = 1:nrow(df_in)+1,
                                    type = "between", rule = c(61, 80.9), style = "light_green")
      wb$add_conditional_formatting("Data",
                                    cols = cols, rows = 1:nrow(df_in)+1,
                                    type = "between", rule = c(41, 60.9), style = "yellow")
      wb$add_conditional_formatting("Data",
                                    cols = cols, rows = 1:nrow(df_in)+1,
                                    type = "between", rule = c(21, 40.9), style = "orange")
      wb$add_conditional_formatting("Data",
                                    cols = cols, rows = 1:nrow(df_in)+1,
                                    type = "between", rule = c(0, 20.9), style = "red")

      #replace everything that isnt supposed to be NA (i.e. character cells) with their original value
      for (cn in seq_len(ncol(df_original))) {
        for (rn in seq_len(nrow(df_original))) {
          if (is.na(df[rn,cn])) {
            wb$add_data("Data", as.character(df_original[rn,cn]), start_col = cn, start_row = rn+1)
          }
        }
      }
        
      #save the workbook
      wb_save(wb, file = paste0(file_name,".xlsx"), overwrite = T)
        
    }
      
    #run formatting on df
    cond_format_num(df_in = df, output = file_name)

    #----- End Method 1
      
    #----- Method 2: return original cell value and associated letter grade with coloured background
      
  } else if (method == "letter"){ 
      
    #create function that determines letter and binds it to the value
    letters_on_grade <-  function(df_in, col) {
      
      #get the column name based on the column index
      col_name <- colnames(df_in[col])
        
      df_in %>% mutate("{col_name}_grade" := case_when(df_in[col] < 21 ~ "(E)",
                                                       df_in[col] >= 21 & df_in[col] < 41 ~ "(D)",
                                                       df_in[col] >= 41 & df_in[col] < 61 ~ "(C)",
                                                       df_in[col] >= 61 & df_in[col] < 81 ~ "(B)",
                                                       df_in[col] >= 81 & df_in[col] < 101 ~ "(A)",
                                                       TRUE ~ ""))
        
    }
      
    #create a counter that starts at the first col designated by the user input
    x <- cols[1]
    
    #determine the index of the first new column that will be created by the loop
    y <- ncol(df) + 1
      
    #run the letter function the same number of times as there is columns to target, joining the outputted letter col back to the inputted score col each loop
    for (i in 1:length(cols)){

      #run the letter function starting on column x until column n
      df <- letters_on_grade(df, x)
        
      #coerce inputted score col, and outputted letter col to character
      df[x] <- lapply(df[x], function(.){as.character(.)})

      #get the name for the column that was input
      col_name <- as.character(colnames(df[x]))
        
      #combine the inputted score col and the outputted letter col with the updated col name
      df <- df %>% unite({{col_name}},  x, y, sep = " ", remove = T)
        
      #increase the column counter so that the next col is inputted before starting again
      x = x + 1
    }
      
    #add the new data to the workbook, over riding the old data that was added
    wb$add_data("Data", df)
      
    #formatting for letter grades (for both positive and negative number ranges)
    cond_format_let <- function(df_in, output) {
        
      #apply the conditional formatting rules to the data
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in)+1,
                                    type = "contains", rule = "(A)", style = "dark_green")
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in)+1,
                                    type = "contains", rule = "(B)", style = "light_green")
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in)+1,
                                    type = "contains", rule = "(C)", style = "yellow")
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in)+1,
                                    type = "contains", rule = "(D)", style = "orange")
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in)+1,
                                    type = "contains", rule = "(E)", style = "red")
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in)+1,
                                    type = "contains", rule = "NA", style = "grey")
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in),
                                    rule = "ISNA(A1)", style = "grey")
      wb$add_conditional_formatting("Data",
                                    cols = 1:ncol(df_in), rows = 1:nrow(df_in)+1,
                                    type = "contains", rule = ".", style = "neutral")
        
        
      #save the workbook
      wb_save(wb, path = paste0(output,".xlsx"), overwrite = T)
    }
      
    #run letter conditional formatting function on the data
    cond_format_let(df_in = df, output = file_name)
      
  } #----- End Method 2

} 