#' Helper to Select Various Colour Schemes
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

  if (scheme == "report_card"){#create a list of report card colours to add to the workbook

    colours_list <- list(
      font_colour = c("#000000", "#000000", "#000000", "#000000", "#000000"),
      bkgd_colour = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF0000"),
      colour_name = c("dark_green", "light_green", "yellow", "orange", "red")
    )

  } else if (scheme == "rainfall"){#create a list of rainfall colours to add to the workbook

    colours_list <- list(
      font_colour = c("#8C510A", "#D8B365", "#F6E8C3", "#F5F5F5", "#C7EAE5", "#5AB4AC", "#01665E"),
      bkgd_colour = c("#8C510A", "#D8B365", "#F6E8C3", "#F5F5F5", "#C7EAE5", "#5AB4AC", "#01665E"),
      colour_name = c("lowest", "very_much_below", "below", "average", "above", "very_much_above", "highest")
    )

  } else if (scheme == "temperature"){#create a list of temperature colours to add to the workbook

    colours_list <- list(
      font_colour = c("#2166AC", "#67A9CF", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#EF8A62", "#B2182B"),
      bkgd_colour = c("#2166AC", "#67A9CF", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#EF8A62", "#B2182B"),
      colour_name = c("lowest", "very_much_below", "below", "average", "above", "very_much_above", "highest")
    )

  } else if (scheme == "summary_statistic"){#create a list of summary stat colours to add to the workbook

    colours_list <- list(
      font_colour = c("#000000", "#000000"),
      bkgd_colour = c("#BDD7EE", "#F8CBAD"),
      colour_name = c("blue", "orange")
    )

  } else if (scheme == "presence_absence"){#create a list of summary stat colours to add to the workbook

    colours_list <- list(
      font_colour = c("#000000", "#000000"),
      bkgd_colour = c("#00B0F0", "#D9D9D9"),
      colour_name = c("blue", "grey")
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