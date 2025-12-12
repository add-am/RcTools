#' Conditional Formatting with .xlsx Output
#'
#' @param Region SF Object. An sf object defining the area to extract data from
#' @param StartDate POSIX time. Supplied as a string in the format YYYY-MM-DD that defines the first day to extract data from
#' @param EndDate POSIX time. Supplied as a string in the format YYYY-MM-DD that defines the last day to extract data from
#' @param Variable Character Vector. One or several variables to extract from eReefs, currently accessible: "Turbidity", "Chlorophyll a", "DIN", "NH4", "NO3", "Secchi", "pH", "Wind"
#' @param Downsample Integer. A singular value that defines the level of downsampling to apply to the data (reduces data size and processing time, also reduces resolution)
#'
#' @returns A NetCDF (stars) object or list of NetCDF (stars) objects
#'
#' @export
#' @examples
#' \dontrun{
#' nc_output <- ereefs_extract(
#'   Region = an_sf_object,
#'   StartDate = "2022-03-01",
#'   EndDate = "2022-03-03",
#'   Variable = "Turbidity",
#'   Downsample = 0)
#' }
ereefs_extract <- function(Region, StartDate, EndDate, Variable, Downsample = 0){

  #check required argument (all of them)
  if (any(missing(Region), missing(StartDate), missing(EndDate), missing(Region), missing(Downsample))){
    stop("You must supply all parameters")
  }

  #check argument types
  if (!inherits(Region, "sf")){stop("You must supply an sf object to the 'Region' parameter")}
  if (!is.numeric(Downsample)){stop("You must supply an numeric argument to the 'Region' parameter")}
  
  #check dates (this will fail if not already a date object or not provided as the correct characters and gives an informative error message)
  StartDate <- as.Date(StartDate)
  EndDate <- as.Date(EndDate)
  
  #check variables by comparing to allowed variables (at the moment)
  var_choices <- c("Turbidity", "Chl_a_sum", "DIN", "NH4", "NO3", "Secchi", "PH", "Wind")
  if (!Variable %in% var_choices){
    stop("Invalid 'Variable': must be one of ", paste(var_choices, collapse = ", "))
  }

  #if wind is an argument, replace it with the four variables
  if (any(stringr::str_detect(Variable, "Wind"))){
    Variable <- c(
      Variable[-which(stringr::str_detect(Variable, "Wind"))], 
      c("wind_dir", "wind_mag", "wind_u", "wind_v"))
  }
  
  #define input link
  input_file <- "https://dapds00.nci.org.au/thredds/dodsC/fx3/GBR1_H2p0_B3p2_Cfur_Dnrt.ncml"

  #convert the sf object into a bounding box, then rearrange the order for how eReefs likes it
  Region <- sf::st_bbox(Region)
  Region <- c(Region[1], Region[3], Region[2], Region[4])

  #get all grids
  grids <- ereefs::get_ereefs_grids(input_file)
        
  #get x and y specifically
  x_grid <- grids[["x_grid"]]
  y_grid <- grids[["y_grid"]]
      
  #if the value is inside the bounds of each of our coords, change it to TRUE. Those outside are automatically false
  true_false_array <- 
    x_grid >= Region[1] & 
    x_grid <= Region[2] & 
    y_grid >= Region[3] & 
    y_grid <= Region[4]
      
  #if the value is NA, change it to false.
  true_false_array[is.na(true_false_array)] <- FALSE

  #return the row index for every row that contains at least one true value:
  true_rows <- which(apply(true_false_array, 1, any))

  #find the first row that contains a true value
  first_row <- true_rows[1]

  #find the number of rows that contains a true value
  num_of_rows <- utils::tail(true_rows, n = 1) - first_row

  #return the row index for every row that contains at least one true value:
  true_cols <- which(apply(true_false_array, 2, any))

  #find the first col that contains a true value
  first_col <- true_cols[1]

  #find the number of cols that contains a true value
  num_of_cols <- utils::tail(true_cols, n = 1) - first_col

  #open the nc file
  nc <- ereefs::safe_nc_open(input_file)
  
  #get a vector of all times
  ds <- as.Date(ereefs::safe_ncvar_get(nc, "time"), origin = as.Date("1990-01-01"))

  #close the nc file
  ncdf4::nc_close(nc)

  #compare the start and stop days against the vector
  StartDiff <- abs(StartDate - ds)
  EndDiff <- abs(EndDate - ds)
    
  #warn the user
  if (min(StartDiff) > 1) {
    warning(paste("Target Start Date", StartDate, "is", min(StartDiff), "days from closest available date of:", ds[which.min(StartDiff)]))
  }
    
  #warn the user
  if (min(EndDiff) > 1) {
    warning(paste("Target End Date", EndDate, "is", min(EndDiff), "days from closest available date of:", ds[which.min(EndDiff)]))
  }
              
  #get the index for the closest days
  StartDateLayerIndex <- which.min(abs(StartDate - ds))
  EndDateLayerIndex <- which.min(abs(EndDate - ds))
  DayCount <- EndDateLayerIndex - StartDateLayerIndex

  #if the user wants secchi or wind these don't have a depth layer
  if (stringr::str_detect(Variable, "Secchi|Wind")){

    #extract data using indices to define layer counts
    nc_data <- stars::read_ncdf(
      input_file, 
      var = Variable,
      downsample = Downsample,
      ncsub = cbind(
        start = c(first_row, first_col, StartDateLayerIndex), 
        count = c(num_of_rows, num_of_cols, DayCount)
      )
    )
  } else {

    #extract data using indices to define layer counts plus include depth
    nc_data <- stars::read_ncdf(
      input_file, 
      var = Variable,
      downsample = Downsample,
      ncsub = cbind(
        start = c(first_row, first_col, 44, StartDateLayerIndex), 
        count = c(num_of_rows, num_of_cols, 1, DayCount)
      )
    )
  }

  #overwrite erroneous high values (note that a value of even 50 would be very very high)
  nc_data[(nc_data > 2000)] <- NA

  #drop dimensions with only 1 value (i.e. the depth dimension) as it doesn't contribute to the data and only makes things harder
  nc_data <- nc_data[drop = TRUE]

  #extract units from the input
  data_unit <- ncmeta::nc_atts(input_file, Variable) |> 
    dplyr::filter(name == "units")

  #keep just the units
  data_unit <- data_unit$value[[1]]

  #put units into the data, they are not carried over well in stars objects so we will hide them in the attribute name
  names(nc_data) <- paste0(Variable, " (", data_unit, ")")

  #return the final dataset
  nc_data
}
 
