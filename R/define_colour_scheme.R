#' Title
#'
#' @param workbook A excel workbook object created by openxlsx2
#' @param scheme String. One of several options: Report Card, Rainfall, Temperature, Summary Stat, TBD.
#'
#' @returns The supplied workbook object, now with the colour scheme embedded
#'
#' @examples
#' \dontrun{
#' wb <- openxlsx2::wb_workbook()
#' define_colour_scheme(workbook = wb, scheme = "Report Card")}
#' 
define_colour_scheme <- function(workbook, scheme){

  #standardise inputs
  scheme <- stringr::str_to_lower(scheme)
  if (scheme == "report card"){#create a list of report card colours to add to the workbook

    colours_list <- list(
      font_colour = c("#000000", "#000000", "#000000", "#000000", "#000000"),
      bkgd_colour = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF0000"),
      colour_name = c("dark_green", "light_green", "yellow", "orange", "red")
    )

  } else if (scheme == "rainfall"){#create a list of rainfall colours to add to the workbook

    colours_list <- list(
      font_colour = c("#000000", "#000000", "#000000", "#000000", "#000000"),
      bkgd_colour = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF0000"),
      colour_name = c("dark_green", "light_green", "yellow", "orange", "red")
    )

  } else if (scheme == "temperature"){#create a list of temperature colours to add to the workbook

    colours_list <- list(
      font_colour = c("#000000", "#000000", "#000000", "#000000", "#000000"),
      bkgd_colour = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF0000"),
      colour_name = c("dark_green", "light_green", "yellow", "orange", "red")
    )

  } else if (scheme == "summary stat"){#create a list of summary stat colours to add to the workbook

    colours_list <- list(
      font_colour = c("#000000", "#000000", "#000000", "#000000", "#000000"),
      bkgd_colour = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF0000"),
      colour_name = c("dark_green", "light_green", "yellow", "orange", "red")
    )

  }

  #for each item in the list, add it to the workbook
  for (i in seq_along(colours_list$font_colour)) {
    workbook$styles_mgr$add(
      openxlsx2::create_dxfs_style(
        font_color = openxlsx2::wb_colour(colours_list$font_colour[i]),
        bgFill = openxlsx2::wb_colour(colours_list$bkgd_colour[i])
      ),
      colours_list$colour_name[i]
    )
  }

  return(workbook)
  
}