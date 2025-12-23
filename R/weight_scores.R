#' Function that weights scores based on predetermined weighting values
#'
#' @param df Dataframe. The table that contains the value(s) you are interested in.
#' @param score Numeric column(s). Any number of numeric columns that contain scores to be weighted.
#' @param weighting Numeric column. A singular numeric column that contains weighting values, must be the same length 
#' as the score column(s). Weights should be decimal (i.e. a 25% weighting should be written as 0.25).
#' 
#' @returns The original dataframe with new weighted scores added. Names are inherited from the targeted columns.
#'
#' @export
#' @examples
#' 
#' x <- data.frame(
#'  Basin = c("Black", "Ross", "Haughton", "Suttor"),
#'  Indicator = c("DIN", "DIN", "Low_DO", "Low_DO"),
#'  Score = c(100, 55, 67, 18),
#'  Weight = c(0.25, 0.5, 0.3, 0.2)
#' )
#' 
#' x <- x |> 
#'   weight_score(
#'     score = Score,
#'     weighting = Weight
#'   )
#'  
weight_score <- function(df, score, weighting){
  
  #check required arguments
  if (missing(df)) {stop("You must supply a dataframe")}
  if (missing(score)) {stop("You must supply at least one score column")}
  if (missing(weighting)) {stop("You must supply a weighting column")}

  df |> 
    dplyr::mutate(
      dplyr::across(
        {{ score }},
        ~ round(.x * {{ weighting }}, 2),
        .names = "{.col}Weighted"
      )
    )

}



x <- data.frame(
 Basin = c("Black", "Ross", "Haughton", "Suttor"),
 Indicator = c("DIN", "DIN", "Low_DO", "Low_DO"),
 Score = c(100, 55, 67, 18),
 Weight = c(0.25, 0.5, 0.3, 0.2)
)

