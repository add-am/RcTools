#' Function that converts values to score
#'
#' @param value Numeric Vector. Any vector of numbers that are values.
#' @param value_type String. Defines the type of values that will be scored. One of: "water quality", "wetlands", "mangroves and saltmarsh"
#' @param indicator String Vector. Defines the indicator that will be scored. Only relevant for water quality and fish. Provide as a vector of equal length to values.
#' @param water_type String. Defines the type of water quality scoring to use. One of "Freshwater", "Estuarine", or "Marine"
#' @param wqo Integer. The water quality objective. Only relevant for water quality
#' @param sf Integer.  The scaling factor. Only relevant for freshwater and estuarine water quality
#' @param eightieth Integer. The eightieth percentile. Only relevant for freshwater and estuarine water quality
#' @param twentieth Integer. The twentieth percentil. Only relevant for freshwater and estuarine water quality
#' @returns A vector. Code suggesting provided for imbedding the vector back into a df
#'
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(
#'  Basin = c("Black", "Ross", "Haughton", "Suttor"),
#'  Value = c(, 76, 27, 98)
#' )
#' x <- x |> 
#'   mutate(across(
#'     Score,
#'     .names = "{.col}_Score",
#'     ~ value_to_score(.)))
#'  
#' }
value_to_score <- function(value, value_type, indicator = NULL, water_type = NULL, wqo = NULL, sf = NULL, eightieth = NULL, twentieth = NULL){

  #clean value type inputs
  value_type <- stringr::str_to_lower(value_type)

  if (value_type == "water quality"){

    #clean indicator inputs
    indicator <- stringr::str_replace_all(stringr::str_to_lower(indicator), "_", " ")

    #clean water type inputs
    water_type <- stringr::str_replace_all(stringr::str_to_lower(water_type), "_", " ")

    #start the land based water quality scoring
    if (water_type %in% c("freshwater", "estuarine")){

      #check for the required secondary inputs
      if (any(is.null(indicator), is.null(wqo), is.null(sf), is.null(eightieth), is.null(twentieth))){
        stop("Please supply all required arguements to score water quality values")
      }
      wqo        <- suppressWarnings(as.numeric(wqo))
      sf         <- suppressWarnings(as.numeric(sf))
      eightieth  <- suppressWarnings(as.numeric(eightieth))
      twentieth  <- suppressWarnings(as.numeric(twentieth))
      value      <- suppressWarnings(as.numeric(value))

      #calculate scores, reverse method if indicator is low do
      ifelse(
        indicator == "low do",
        return(
          ifelse(
            value > wqo, pmax(60.9 - (60.9 * (abs((value - wqo)/(sf - wqo)))), 0), #scores from 0 to 61
            ifelse(value <= wqo & eightieth > wqo, 80.99 - (19.9 * (abs((eightieth - wqo)/(eightieth - value)))), 90) #scores from 61 to 81, then 90
          )
        ),
        return(  
          ifelse(
            value < wqo, pmax(60.9 - (60.9 * (abs((value - wqo)/(sf - wqo)))), 0), #scores from 0 to 61
            ifelse(value >= wqo & twentieth < wqo, 80.99 - (19.9 * (abs((wqo - twentieth)/(value - twentieth)))), 90) #scores from 61 to 81, then 90
          )
        )
      )

    }

    if (water_type == "marine"){

      #check for the required secondary inputs
      if (any(is.null(indicator), is.null(wqo))){
        stop("Please supply all required arguements to score water quality values")
      }

      #calculate scores, reverse method if indicator is secchi depth
      if (!stringr::str_detect(indicator, "secchi")){

        val <- ifelse(log2(wqo/value) <= -1, -1, #if Wqo/Value is less than -1, cap at -1
               ifelse(log2(wqo/value) >= 1, 1, log2(wqo/value))) #if Wqo/Value is greater than 1, cap at 1, else keep calculated value.

      } else {

        val <- ifelse(log2(value/wqo) <= -1, -1, #if Value/Wqo is less than -1, cap at -1
               ifelse(log2(value/wqo) >= 1, 1, log2(value/wqo))) #if Value/Wqo is greater than 1, cap at 1, else keep calculated value.

      }

      #convert the -1 to +1 range to a score from 0 to 100

      return(
        ifelse(val >= 0.51, 100 - (19 - ((val - 0.51) * (19/0.49))), #scores from 100 to 81
        ifelse(val >= 0 & val < .51, 80.9 - (19.9 - (val * (19.9/0.50))), #scores from 80 to 61
        ifelse(val >= -0.33 & val < -0.01, 60.9 - (19.9 - ((val + 0.33) * (19.9/0.32))), #scores from 60 to 41
        ifelse(val >= -0.66 & val < -0.34, 40.9 - (19.9 - ((val + 0.66) * (19.9/0.32))), 20.9 - (20.9 - ((val + 1) * (20.9/0.34))))))) #scores from 40 down
      )
    
    }
  }

  if (value_type %in% c("wetlands", "mangroves and saltmarsh")){

    return(
      ifelse(value > 0, floor(100-abs(19-((abs(value)-0)*(19/99.9)))), #scores from 100 to 81
      ifelse(value >= -0.1, floor(61+abs(19.9-((abs(value)-0)*(19.9/0.1)))), #scores from 80 to 61
      ifelse(value >= -0.5, floor(41+abs(19.9-((abs(value)-0.11)*(19.9/0.39)))), #scores from 60 to 41
      ifelse(value >= -3, floor(21+abs(19.9-((abs(value)-0.51)*(19.9/2.49)))), floor(abs(20.9-((abs(value)-3.01)*(20.9/96.99)))))))) #scores from 40 down to 0
    )

  }

  if (value_type == "riparian"){

    return(
      ifelse(value > 0, floor(100-abs(19-((abs(value)-0)*(19/99.9)))), #scores from 100 to 81
      ifelse(value >= -0.1, floor(61+abs(19.9-((abs(value)-0)*(19.9/0.1)))), #scores from 80 to 61
      ifelse(value >= -0.5, floor(41+abs(19.9-((abs(value)-0.11)*(19.9/0.39)))), #scores from 60 to 41
      ifelse(value >= -1, floor(21+abs(19.9-((abs(value)-0.51)*(19.9/0.49)))), floor(abs(20.9-((abs(value)-1.01)*(20.9/98.99))))))))  #scores from 40 down to 0
    )

  }

  if (value_type == "fish"){

    #clean indicator inputs
    indicator <- stringr::str_replace_all(stringr::str_to_lower(indicator), "_", " ")

    #check for the required secondary inputs
    if (is.null(indicator)){
      stop("Please supply all required arguements to score fish values")
    }

    ifelse(
      indicator == "poise",
      return(
        ifelse(value > 0.8, 81 + abs((19 + ((value - 1) * (19 / 0.2)))), #scores from 100 to 81
        ifelse(value > 0.67, 61 + abs((19.9 + ((value - 0.7999) * (19.9 / 0.1329)))), #scores from 80 to 61
        ifelse(value > 0.53, 41 + abs((19.9 + ((value - 0.6669) * (19.9 / 0.1339)))), #scores from 60 to 41
        ifelse(value > 0.4, 21 + abs((19.9 + ((value - 0.5329) * (19.9 / 0.1329)))), abs(20.9 + ((value - 0.3999) * (20.9 / 0.3999))))))) #scores from 40 down
      ),
      return(
        ifelse(value < 0.03, 81 + abs((19 - ((value - 0) * (19 / 0.025)))), #scores from 100 to 81
        ifelse(value < 0.05, 61 + abs((19.9 - ((value - 0.0251) * (19.9 / 0.0249)))), #scores from 80 to 61
        ifelse(value < 0.1, 41 + abs((19.9 - ((value - 0.051) * (19.9 / 0.049)))), #scores from 60 to 41
        ifelse(value < 0.2, 21 + abs((19.9 - ((value - 0.101) * (19.9 / 0.099)))), abs(20.9 - ((value - 0.201) * (20.9 / 0.799))))))) #scores from 40 down
      )
    )              
  }

}
