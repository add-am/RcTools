#' Function that converts scores to grades
#'
#' @param score Numeric Vector. Any vector of numbers that are scores, usually from 0 to 100
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
#'     ~ score_to_grade(.)))
#'  
#' }
score_to_grade <- function(score, grading_type){

  ifelse(score >= 81, "A",
  ifelse(score >= 61, "B",
  ifelse(score >= 41, "C",
  ifelse(score >= 21, "D", "E"))))

}