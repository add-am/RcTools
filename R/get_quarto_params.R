#' Return the YAML params during an interative session
#'
#' @returns a list of all YAML parameters
#'
#' @export
#' @examples
#' 
#' params <- get_quarto_params()
#' 
get_quarto_params <- function(){

  #if the session calling this function is interactive
  if (interactive()) {
    params <- rmarkdown::yaml_front_matter(rstudioapi::getSourceEditorContext()$path)$params
  }

  #return the params object
  return(params)

}