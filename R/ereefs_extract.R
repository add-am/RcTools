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
ereefs_extract <- function(Region, StartDate, EndDate, Variable, Downsample){

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
  var_choices <- c("Turbidity", "Chlorophyll a", "DIN", "NH4", "NO3", "Secchi", "pH", "Wind")
  if (!Variable %in% var_choices){
    stop("Invalid 'Variable': must be one of ", paste(var_choices, collapse = ", "))
  }

  #if wind is an argument, replace it with the four variables
  if (any(stringr::str_detect(Variable, "Wind"))){
    Variable <- c(
      Variable[-which(stringr::str_detect(Variable, "Wind"))], 
      c("Wind Direction", "Wind Stress", "Wind U Component", "Wind V Component"))
  }

  #rename all arguments into the format ereefs likes
  var_rename <- c(
    "Turbidity" = "Turbidity", "Chlorophyll a" = "Chl_a_sum", "DIN" = "DIN", "NH4" = "NH4", "NO3" = "NO3", "Secchi" = "Secchi", 
    "pH" = "PH", "Wind Direction" = "wind_dir", "Wind Stress" = "wind_mag", "Wind U Component" = "wind_u", "Wind V Component" = "wind_v"
  )

  #create a new vector of these ereefs names
  Variable <- unname(var_rename[Variable])
  
  #define input link
  input_file <- "https://dapds00.nci.org.au/thredds/dodsC/fx3/GBR1_H2p0_B3p2_Cfur_Dnrt.ncml"

  #turn off spherical geometry
  sf::sf_use_s2(FALSE)

  #simplify and summarise the sf object
  Region <- Region |> 
    dplyr::summarise(geom = st_union(geom)) |> 
    dplyr::ungroup() |> 
    sf::st_cast() |> 
    sf::st_make_valid()

  #convert the sf object into a bounding box, then rearrange the order for how eReefs likes it
  Region <- st_bbox(Region)
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
  num_of_rows <- tail(true_rows, n = 1) - first_row

  #return the row index for every row that contains at least one true value:
  true_cols <- which(apply(true_false_array, 2, any))

  #find the first col that contains a true value
  first_col <- true_cols[1]

  #find the number of cols that contains a true value
  num_of_cols <- tail(true_cols, n = 1) - first_col

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

  #extract data using indices to define layer counts
  nc_data <- stars::read_ncdf(
    input_file, 
    var = Variable,
    downsample = Downsample,
    ncsub = cbind(
      start = c(first_row, first_col, 44, StartDateLayerIndex), 
      count = c(num_of_rows, num_of_cols, 1, DayCount)
    )
  )

  #overwrite erroneous high values (note that a value of even 50 would be very very high)
  nc_data[(nc_data > 2000)] <- NA

  #update the crs on the data (move from lat long to meters)
  nc_data <- nc_data |> sf::st_transform("EPSG:7855")

  #convert our curvilinear object into just a bbox
  curvilinear_bbox <- nc_data |> 
    sf::st_bbox() |>
    sf::st_as_sfc()

  #get a linear grid target with the same dimensions (number of cells) as our curvilinear grid 
  reg_stars <- stars::st_as_stars(
    curvilinear_bbox, #using the bbox to provide the xmin, xmax etc., 
    nx = dim(nc_data)[[1]], #and the dimensions to provide the x and y count. 
    ny = dim(nc_data)[[2]], 
    values = NA_real_) #Fill each cell with NA

  #run st warp, it requires a curvilinear object, and a regular object as a target
  warped_data <- stars::st_warp(nc_data, reg_stars)

  #drop the depth dimension as this only has one layer
  final_data <- warped_data[drop = TRUE]

  #return the final dataset
  final_data
}


my_sf <- sf::st_read("n3_region.gpkg")

my_sf <- my_sf |> 
  dplyr::filter(Region == "Dry Tropics", Environment == "Marine")




test <- ereefs_extract(
  my_sf,
  StartDate = "2016-03-01",
  EndDate = "2022-03-03",
  Variable = "Turbidity",
  Downsample = 0
)



  
