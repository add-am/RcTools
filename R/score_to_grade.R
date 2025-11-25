#' Function that converts scores to grades
#'
#' @param df Dataframe. The table that contains the score(s) you are interested in.
#' @param score_cols Numeric column(s). Any number of numeric columns that contain scores. Values are expected to
#' range from 0 to 100
#' 
#' @returns The original dataframe with new grade columns added. Names are inherited from the targeted columns.
#'
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(
#'  Basin = c("Black", "Ross", "Haughton", "Suttor"),
#'  ScoreA = c(51, 76, 27, 98),
#'  ScoreB = c(7, 42, 89, 63),
#'  ScoreC = c(15, 34, 56, 21),
#' )
#' x <- score_to_grade(x, c("ScoreA", "ScoreB", "ScoreC"))
#'   
#' }
score_to_grade <- function(df, score_cols){

  df |> 
    dplyr::mutate(
      dplyr::across(
        {{ score_cols }},
        ~ dplyr::case_when(
          .x >= 81 ~ "A",
          .x >= 61 ~ "B",
          .x >= 41 ~ "C",
          .x >= 21 ~ "D",
          T ~ "E"
        ),
        .names = "{.col}Grade"
      )
    ) 

}
