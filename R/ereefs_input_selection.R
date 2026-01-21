#' Provide a Range of Models to Choose From
#'
#' @param input_file Character string. Defaults to "catalog" however can also accept a premade url
#'
#' @returns A Character string url to one of 12 models.
#'
#' @examples
#' \dontrun{ #dont run because function is not exported
#' url <- ereefs_input_selection()
#' }
#' 
ereefs_input_selection <- function(input_file = "catalog") {

  #open the catalog and get a vector of dataset names to choose from  
  catalog <- thredds::get_catalog("https://dapds00.nci.org.au/thredds/catalogs/fx3/catalog.xml")
  choices <- catalog$get_dataset_names()

  #create a vector of all possible urls based on the 12 names
  all_possible_urls <- purrr::map_chr(seq(choices), \(x) {

    #build the full url
    url <- paste0(
      stringr::str_extract(catalog$url, "https://.[^/]*"), 
      catalog$list_services()$ncdods[3], 
      catalog$get_datasets()[[x]]$get_url()
    )

  })

  #if the user did not provide the default catalog and also did not provided a url that exists in the 12
  if (input_file != "catalog" & !any(stringr::str_detect(input_file, all_possible_urls))){

    stop("You must provide either a valid url, or use the default 'catalog' input for the requested model.")

    #else if the user did provide the default catalog, ask them which url they want
  } else if (input_file == "catalog"){
  
    print("Refer to https://research.csiro.au/ereefs/models/models-about/biogeochemical-simulation-naming-protocol/ for naming conventions.")
    
    selection <- choices[utils::menu(choices)]

    url <- paste0(
      stringr::str_extract(catalog$url, "https://.[^/]*"), 
      catalog$list_services()$ncdods[3], 
      catalog$get_datasets()[[selection]]$get_url()
    )

    #return their original url
  } else {
    url <- input_file
  }
 
  #return the url
  return(url)
}