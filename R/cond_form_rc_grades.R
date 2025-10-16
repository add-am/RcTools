#inputs: 
#df <- any tbl or data.frame
#file name <- whatever you want the output file to be named
#cols <- the index number of each column that the function should be applied to
#method <- either "numeric" or "letter". numeric only returns numbers, letter will return numbers and the associated letter grade.

cond_form_rc_grades <- function(df, file_name, cols, method = "numeric"){
  
  #----- Data frame preparation
  
  #make lowercase and clean up the indicator variable
  method <- stringr::str_to_lower(method)
  
  #create a duplicate that doesn't get all columns converted to numeric
  df_original <- df

  #coerce cols to numeric - cols may not be numeric if they contain "weird" Nan, NA, or ND values
  df[cols] <- lapply(df[cols], function(x){suppressWarnings(as.numeric(x))})
  
  #----- Excel workbook preparation
  
  #create an empty workbook
  wb <- openxlsx2::wb_workbook()
  
  #add the data to the workbook
  wb$add_worksheet("Data")
  wb$add_data("Data", df)
  
  #create the cell style we want
  dgn <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_colour("#000000"), bgFill = openxlsx2::wb_colour("#00B050")) #dark green
  lgn <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_colour("#000000"), bgFill = openxlsx2::wb_colour("#92D050")) #light green
  ylw <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_colour("#000000"), bgFill = openxlsx2::wb_colour("#FFFF00")) #yellow
  orn <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_colour("#000000"), bgFill = openxlsx2::wb_colour("#FFC000")) #orange
  red <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_colour("#000000"), bgFill = openxlsx2::wb_colour("#FF0000")) #red
  gry <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_colour("#000000"), bgFill = openxlsx2::wb_colour("#D9D9D9")) #grey
  neu <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_colour("#000000"), bgFill = openxlsx2::wb_colour("#FFFFFF")) #neutral (white)
  
  #put the style into the styles manager of the workbook
  wb$styles_mgr$add(dgn, "dark_green")
  wb$styles_mgr$add(lgn, "light_green")
  wb$styles_mgr$add(ylw, "yellow")
  wb$styles_mgr$add(orn, "orange")
  wb$styles_mgr$add(red, "red")
  wb$styles_mgr$add(gry, "grey")
  wb$styles_mgr$add(neu, "neutral")
  
  #if user asks for numeric, run numeric else, run letter
  if (method == "numeric"){
    cond_form_numeric()
  } else {
    cond_form_letter()
  }

} 