#' Function that converts scores to grades
#'
#' @param score Numeric Vector. Any vector of numbers that are scores, usually from 0 to 100
#' @param grading_type String. Defining the type of scoring to use
#'
#' @returns A vector. Code suggesting provided for imbedding the vector back into a df
#'
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(
#'  Basin = c("Black", "Ross", "Haughton", "Suttor"),
#'  Score = c(51, 76, 27, 98)
#' )
#' x <- x |> 
#'   mutate(across(
#'     Score,
#'     .names = "{.col}_Grade",
#'     ~ score_to_grade(., "Water Quality")))
#'  
#' }
score_to_grade <- function(score, grading_type){

  if (grading_type == "Water Quality" {

  } else if () {
    
  }

    
}