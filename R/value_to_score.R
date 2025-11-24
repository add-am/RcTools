#' Function that converts values to score
#'
#' @param df Dataframe. The table that contains the value(s) you are interested in.
#' @param value Numeric column(s). Any number of numeric columns that contain values to be scored. Values ranges can vary 
#' significantly depending on the indicator.
#' @param value_type String. Defines the type of values that will be scored. One of: "water quality", "wetlands", 
#' "mangroves and saltmarsh".
#' @param indicator String column. Defines the indicator that will be scored. Only relevant for water quality and fish. 
#' Should be sourced from the same df as the value column(s).
#' @param water_type String. Defines the type of water quality scoring to use. One of "Freshwater", "Estuarine", or "Marine"
#' @param wqo Numeric column. The water quality objective associated with each value. Only relevant for water quality. 
#' Should be sourced from the same df as the value column(s).
#' @param sf Numeric column. The scaling factor associated with each value. Only relevant for freshwater and estuarine 
#' water quality. Should be sourced from the same df as the value column(s).
#' @param eightieth Numeric column. The eightieth percentile associated with each value. Only relevant for freshwater 
#' and estuarine water quality. Should be sourced from the same df as the value column(s).
#' @param twentieth Numeric column. The twentieth percentile associated with each value. Only relevant for freshwater 
#' and estuarine water quality. Should be sourced from the same df as the value column(s).
#' 
#' @returns The original dataframe with new score columns added. Names are inherited from the targeted columns.
#'
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(
#'  Basin = c("Black", "Ross", "Haughton", "Suttor"),
#'  Indicator = c("DIN", "DIN", "Low_DO", "Low_DO"),
#'  Value = c(0.002, 0.017, 87.6, 104),
#'  Wqo = c(0.02, 0.02, 90, 90),
#'  Sf = c(0.38, 0.38, 70, 70),
#'  Eightieth = c(0.0154, 0.0154, 101.12, 101.12),
#'  Twentieth = c(0.0026, 0.0026, 75.84, 75.84)
#' )
#' 
#' x <- x |> 
#'   value_to_score(
#'     value_type = "Water Quality",
#'     water_type = "Freshwater",
#'     indicator = Indicator,
#'     wqo = Wqo,
#'     sf = Sf,
#'     eightieth = Eightieth,
#'     twentieth = Twentieth
#'   )
#'  
#' }
value_to_score <- function(
  df, 
  value, 
  value_type, 
  water_type = NULL, 
  indicator = NULL, 
  wqo = NULL, 
  sf = NULL, 
  eightieth = NULL, 
  twentieth = NULL){
  
  #check required arguments
  if (missing(df)) {stop("You must supply a dataframe")}
  if (missing(value)) {stop("You must supply a value column")}
  if (missing(value_type)) {stop("You must supply a value_type column")}

  #check value type input
  value_type_choices <- c("Water Quality", "Wetlands", "Mangroves and Saltmarsh", "Riparian", "Fish")
  if (!(value_type %in% value_type_choices)){
    stop("Invalid value_type: must be one of ", paste(value_type_choices, collapse = ", "))
  }

  #check water type input it can be NULL or one of three, if value type is water quality it must be one of three
  if (value_type == "Water Quality"){
    if (is.null(water_type)){stop("A water type must be supplied when scoring water quality values.")}
    water_type_choices <- c("Freshwater", "Estuarine", "Marine")
    if (!(water_type %in% water_type_choices)){
      stop("Invalid water_type: must be one of ", paste(water_type_choices, collapse = ", "))
    }
  }

  #check the supplied indicator is correct (only relevant for water quality and fish)
  if (value_type %in% c("Water Quality", "Fish")){
    
    if (is.null(indicator)){stop("An indicator column must be supplied when scoring water quality or fish values.")}

    #extract the actual name of the column that was provided by the user
    indicator_col_name <- rlang::quo_name(rlang::enquo(indicator))

    #standardise the input values
    df <- df |> 
      dplyr::mutate({{ indicator }} := stringr::str_replace_all(stringr::str_to_lower({{ indicator }}), "_", " "))

    #create a list of allowed indicator names
    allowed_indicator_names <- c(
      "din", "tp", "ammonia", "nox", "turbidity", "high do", "low do", 
      "frp", "pn", "pp", "tn", "tss", "secchi", "chla", "poise", "ponis")
      
    #compare the values in the supplied column against the allowed names. keep values that are different
    bad_values <- setdiff(unique(df[[indicator_col_name]]), allowed_indicator_names)

    #if there are any values, stop and return an error
    if (length(bad_values) > 0) {stop("Indicator column contains invalid values: ", paste(bad_values, collapse = ", "))}

  }

  if (value_type == "Water Quality"){

    #all water quality scoring requires at least an indicator column, a value, and a wqo object
    if (!rlang::quo_name(rlang::enquo(indicator)) %in% names(df)){stop("The indicator column does not exists in the dataframe")}
    if (!rlang::quo_name(rlang::enquo(value)) %in% names(df)){stop("The value column does not exists in the dataframe")}
    if (!rlang::quo_name(rlang::enquo(wqo)) %in% names(df)){stop("The wqo column does not exists in the dataframe")}   

    if (water_type %in% c("Freshwater", "Estuarine")){

      #fw and est also require an sf, 80th, and 20th
      if (!rlang::quo_name(rlang::enquo(sf)) %in% names(df)){stop("The sf column does not exists in the dataframe")}
      if (!rlang::quo_name(rlang::enquo(eightieth)) %in% names(df)){stop("The eightieth column does not exists in the dataframe")}
      if (!rlang::quo_name(rlang::enquo(twentieth)) %in% names(df)){stop("The twentieth column does not exists in the dataframe")}

      #complete the score calculation
        df <- df |> 
          dplyr::mutate(
            Score = dplyr::case_when(
              stringr::str_detect({{ indicator }}, "low do") & {{ value }} > {{ wqo }} ~ 
                pmax(60.9 - (60.9 * (abs(({{ value }} - {{ wqo }})/({{ sf }} - {{ wqo }})))), 0), #scores from 0 to 61,
              stringr::str_detect({{ indicator }}, "low do") & {{ value }} <= {{ wqo }} & {{ eightieth }} > {{ wqo }} ~ 
                80.99 - (19.9 * (abs(({{ eightieth }} - {{ wqo }})/({{ eightieth }} - {{ value }})))), #scores from 61 to 81, 
              !stringr::str_detect({{ indicator }}, "low do") & {{ value }} < {{ wqo }} ~ 
                pmax(60.9 - (60.9 * (abs(({{ value }} - {{ wqo }})/({{ sf }} - {{ wqo }})))), 0), #scores from 0 to 61,
              !stringr::str_detect({{ indicator }}, "low do") & {{ value }} >= {{ wqo }} & {{ twentieth }} < {{ wqo }} ~ 
                80.99 - (19.9 * (abs(({{ wqo }} - {{ twentieth }})/({{ value }} - {{ twentieth }})))), #scores from 61 to 81,
              TRUE ~ 90 #otherwise 90
            )
          ) |> 
          dplyr::mutate(Score = round(Score, 3))
        
        return(df)

    } 
    
    if (water_type == "Marine") {

      #complete the score calculation
        df <- df |> 
          dplyr::mutate(
            Score = dplyr::case_when(
              stringr::str_detect({{ indicator }}, "secchi") & log2({{ wqo }}/{{ value }}) <= -1 ~ 1,  #if Wqo/Value is less than -1, cap at -1
              stringr::str_detect({{ indicator }}, "secchi") & log2({{ wqo }}/{{ value }}) >= 1 ~ 1, #if Wqo/Value is greater than 1, cap at 1
              stringr::str_detect({{ indicator }}, "secchi") ~ log2({{ wqo }}/{{ value }}), #take the actual value
              !stringr::str_detect({{ indicator }}, "secchi") & log2({{ value }}/{{ wqo }}) <= -1 ~ -1,  #if Value/Wqo is less than -1, cap at -1
              !stringr::str_detect({{ indicator }}, "secchi") & log2({{ value }}/{{ wqo }}) >= 1 ~ 1, #if Value/Wqo is greater than 1, cap at 1
              TRUE ~ log2({{ value }}/{{ wqo }}) #take the actual value
            )
          ) |> 
          dplyr::mutate(
            Score = dplyr::case_when(
              Score >= 0.51 ~ 100 - (19 - ((Score - 0.51) * (19/0.49))), #scores from 100 to 81
              Score >= 0 ~ 80.9 - (19.9 - (Score * (19.9/0.50))), #scores from 80 to 61
              Score >=-0.33 ~ 60.9 - (19.9 - ((Score + 0.33) * (19.9/0.32))),  #scores from 60 to 41
              Score >= -0.66 ~ 40.9 - (19.9 - ((Score + 0.66) * (19.9/0.32))), #scores from 40 to 21
              TRUE ~ 20.9 - (20.9 - ((Score + 1) * (20.9/0.34))) #scores from 20 down
            )
          ) |> 
          dplyr::mutate(Score = round(Score, 3))
        
        return(df)
    }

  }

  if (value_type %in% c("Wetlands", "Mangroves and Saltmarsh")){

    df <- df |> 
      dplyr::mutate(
        Score = dplyr::case_when(
          {{ value }} > 0 ~ 100-abs(19-((abs({{ value }})-0)*(19/99.9))),
          {{ value }} >= -0.1 ~61+abs(19.9-((abs({{ value }})-0)*(19.9/0.1))),
          {{ value }} >= -0.5 ~ 41+abs(19.9-((abs({{ value }})-0.11)*(19.9/0.39))),
          {{ value }} >= -3 ~ 21+abs(19.9-((abs({{ value }})-0.51)*(19.9/2.49))),
          TRUE ~ abs(20.9-((abs({{ value }})-3.01)*(20.9/96.99)))
        )
      ) |> 
      dplyr::mutate(Score = round(Score, 3))
    
    return(df)

  }

  if (value_type == "Riparian"){

    df <- df |> 
      dplyr::mutate(
        Score = dplyr::case_when(
          {{ value }} > 0 ~ 100-abs(19-((abs({{ value }})-0)*(19/99.9))),
          {{ value }} >= -0.1 ~ 61+abs(19.9-((abs({{ value }})-0)*(19.9/0.1))),
          {{ value }} >= -0.5 ~ 41+abs(19.9-((abs({{ value }})-0.11)*(19.9/0.39))),
          {{ value }} >= -1 ~ 21+abs(19.9-((abs({{ value }})-0.51)*(19.9/0.49))),
          TRUE ~ abs(20.9-((abs({{ value }})-1.01)*(20.9/98.99)))
        )
      ) |> 
      dplyr::mutate(Score = round(Score, 3))
    
    return(df)

  }

  if (value_type == "Fish"){

    df <- df |> 
      dplyr::mutate(
        Score = dplyr::case_when(
          stringr::str_detect({{ indicator }}, "poise") & {{ value }} > 0.8 ~ 
            81 + abs((19 + (({{ value }} - 1) * (19 / 0.2)))), #scores from 100 to 81
          stringr::str_detect({{ indicator }}, "poise") & {{ value }} > 0.67 ~ 
            61 + abs((19.9 + (({{ value }} - 0.7999) * (19.9 / 0.1329)))), #scores from 80 to 61
          stringr::str_detect({{ indicator }}, "poise") & {{ value }} > 0.53 ~ 
            41 + abs((19.9 + (({{ value }} - 0.6669) * (19.9 / 0.1339)))), #scores from 60 to 41
          stringr::str_detect({{ indicator }}, "poise") & {{ value }} > 0.4 ~ 
            21 + abs((19.9 + (({{ value }} - 0.5329) * (19.9 / 0.1329)))),  #scores from 40 to 21
          stringr::str_detect({{ indicator }}, "poise") ~ 
            pmax(abs(20.9 + (({{ value }} - 0.3999) * (20.9 / 0.3999))), 0), #scores from 20 to 0
          stringr::str_detect({{ indicator }}, "ponis") & {{ value }} < 0.03 ~ 
            pmin(abs((19 - (({{ value }} - 0) * (19 / 0.025)))), 100), #scores from 100 to 81
          stringr::str_detect({{ indicator }}, "ponis") & {{ value }} < 0.05 ~ 
            abs((19.9 - (({{ value }} - 0.0251) * (19.9 / 0.0249)))), #scores from 80 to 61
          stringr::str_detect({{ indicator }}, "ponis") & {{ value }} < 0.1 ~ 
            abs((19.9 - (({{ value }} - 0.051) * (19.9 / 0.049)))), #scores from 60 to 41
          stringr::str_detect({{ indicator }}, "ponis") & {{ value }} < 0.2 ~ 
            abs((19.9 - (({{ value }} - 0.101) * (19.9 / 0.099)))), #scores from 40 to 21
          stringr::str_detect({{ indicator }}, "ponis") ~ 
            pmax(abs(20.9 - (({{ value }} - 0.201) * (20.9 / 0.799))), 0) #scores from 20 to 0
        )
      ) |> 
      dplyr::mutate(Score = round(Score, 3))
    
    return(df)
           
  }
}

