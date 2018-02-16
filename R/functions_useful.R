### SOME USEFUL DATA HANDLING FUNCTIONS ###



#' Add object to a list
#'
#' @description
#' This function adds objects to the end of a list
#' @usage lappend(lst, obj, listname)
#' @details
#' As the list can contain miscellaneous variable types, this function should also work regardless of the variable type. No thorough tests are done, however.
#' @param lst a list to which object is added
#' @param obj an object which is added
#' @param listname name of the added object as it appers in the list
#' @return the original list with the object added
#' @export
#' @examples
#' all_station_lists <- lappend(all_station_lists,loaded_list$wmon,
#' "mos_stations_homogeneous_Europe")
lappend <- function(lst, obj, listname) {
  lst[[length(lst)+1]] <- obj
  names(lst)[[length(lst)]] <- listname
  return(lst)
}



#' #' Test if x not in y
#' #'
#' #' @description This is the opposite function to the \pkg{base} inline function \%in\%
#' #' @usage notin(x, y)
#' #' @details \pkg{base} infix function \%in\% is in reality written as \%in\% <- function(x, table) match(x, table, nomatch = 0) > 0,
#' #' \%notin\% is written simply as \%notin\% <- function(x,y) !(x \%in\% y)
#' #' @param x object which is matched in y
#' #' @param y the list to what x is matched against
#' #' @return
#' #' A vector of the same length as x. A logical vector, indicating if a match was NOT located for each element of x: thus the values are TRUE or FALSE and never NA.
#' #' @export
#' notin <- function(x,y) !x %in% y



#' #' If x is null then return y
#' #'
#' #' @description This is an inline function that provides a default value in cases when the result is NULL
#' #' @usage x \%||\% y
#' #' @details The whole code of this function is `\%||\%` <- function(x, y) if (!is.null(x)) x else y \cr
#' #' Typical use for this inline function is to provide a default value if a function gives NULL, e.g. function_that_might_return_null() \%||\% default value
#' #' @param x Object which may return NULL (most likely a function)
#' #' @param y Default value in case of NULL
#' #' @return Either the value returned by x or default value y
#' #' @export
#' #' @seealso \url{http://adv-r.had.co.nz/Functions.html}
#' `%||%` <- function(x, y) if (!is.null(x)) x else y



#' Converting integers to a boolean list
#'
#' @description Which (in \pkg{base}) converts true/false values to integers, unwhich does the opposite (n corresponds to vector length where true values are assigned to)
#' @usage unwhich(x,n)
#' @details The function first creates n-length list of FALSE values. After this, indices x are replaced with TRUE values.
#' @param x Integers that indicate TRUE values
#' @param n Boolean list length
#' @return Boolean list with length n
#' @export
#' @seealso \url{http://adv-r.had.co.nz/Subsetting.html}
#'
unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}



#' Defining time series with fixed intervals
#'
#' @description Define a regularly spaced time series
#' @usage define_time_series(begin_date, end_date, interval_in_hours)
#' @details Use POSIXct-type date-time stamps! Time series are rounded to nearest even 3-hour ({00,03,06,09,12,15,18,21}). Default values of the parameters correspond to dates used in MOS training (begin_date = "2011-12-01 00:00:00 GMT", end_date = Sys.time()).
#' @param begin_date First date of the time series
#' @param end_date All time stamps in the time series must be smaller than this last value
#' @param interval_in_hours Interval of two consecutive time stamps in the time series
#' @return A regularly spaced time series
define_time_series <- function(begin_date=as.POSIXct("2011-12-01 00:00:00 GMT",tz="GMT"),end_date=with_tz(round.POSIXt(Sys.time(),"hours"),tz="GMT"),interval_in_hours=3,interval_in_seconds=NA,even_hours=TRUE) {
  if (even_hours==TRUE) {
    # First rounding to even hours
    begin_date <- lubridate::with_tz(round.POSIXt(begin_date,"hours"),tz="GMT")
    end_date <- lubridate::with_tz(round.POSIXt(end_date,"hours"),tz="GMT")
    # NOT NEEDED
    # # Rounding timestamps to nearest 3 hour forecast time ({00,03,06,09,12,15,18,21})
    # begin_date <- begin_date - (as.numeric(format(round.POSIXt(begin_date, c("hours")),'%H'))%%3)*3600
    # end_date <- end_date - (as.numeric(format(round.POSIXt(end_date, c("hours")),'%H'))%%3)*3600
  }
  # If interval_in_seconds -parameter is given, it is always used
  if (is.na(interval_in_seconds)) {
    series <- seq(from=begin_date, to=end_date, by=paste0(interval_in_hours," hour"))
  } else {
    series <- seq(from=begin_date, to=end_date, by=paste0(interval_in_seconds," secs"))
  }

  return(series)
}




#' Interpolating (timeseries) values
#'
#' @description This is a generic NA-interpolation switch wrapper which allows the use of several interpolation methods
#' @usage Interpolate_timeseries(data_to_be_interpolated, interpolation_method, maximum_gap, ...)
#' @details data_to_be_interpolated is 1-dimensional (without actual time stamps). It contains NA-values for the missing time stamps. Returned data is the same size but with interpolated values.
#' @param data_to_be_interpolated 1-dimensional data with NA values.
#' @param interpolation_method The selected interpolation method. ("repeat_previous","linear_interp","spline_interp","no_interp") are supported.
#' @param maxgap Maximum gap (number of consecutive NA-values) allowed, which still is being interpolated
#' @param ... Possible additional parameters passed to zoo-package interpolation functions
#' @return Interpolated time series of same size as the input data.
interpolate_NA_values <- function(data_to_be_interpolated, interpolation_method, na.rm=FALSE, ...) {
  if (operator.tools::`%!in%`(interpolation_method, c("repeat_previous","linear_interp","spline_interp","no_interp"))) {
    warning("check obs_interpolation_method! Not interpolating!")
    return(data_to_be_interpolated)
  }
  switch(interpolation_method,
         repeat_previous = zoo::na.locf(object=data_to_be_interpolated, na.rm=na.rm, ...),
         linear_interp = zoo::na.approx(object=data_to_be_interpolated, na.rm=na.rm, ...),
         spline_interp = zoo::na.spline(object=data_to_be_interpolated, na.rm=na.rm, ...),
         no_interp = data_to_be_interpolated)
}



#' A function that tests whether an object is either NULL _or_ a list of NULLs
#'
#' @description This is a small function that tests whether all elements in a list are either NULL values OR a list of NULL values
#' @usage is.NullOb(list)
#' @param list list which is checked
#' @return A logical list of same size as input list
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' A function that recursively removes all only-NULL lists among nested lists
#'
#' @description This function removes all NULL lists from the input data. This function is recursive, so the input list can have as much nested lists as possible.
#' @usage rmNullObs(list)
#' @param list list where NULL values are removed
#' @return A list without NULL values
rmNullObs <- function(x) {
  x <- Filter(Negate(MOSpointutils::is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) MOSpointutils::rmNullObs(x) else x)
}






#' A function for reading in data files
#'
#' @description This function reads in RData files and returns the objects in it to the environment where the function was called from. Using this function the data can be assigned to an arbitrary variable name instead of that used in the RData file itself while it was saved.
#' @usage loadRData(fileName)
#' @param fileName The file which is loaded
#' @return the data stored in .RData file
loadRData <- function(fileName){
  #loads an RData file into the function's own environment and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}





#' A function which calculates the smallest difference between two circularly defined values.
#'
#' @description This function calculates the smallest difference between two circularly defined values. Examples from circular variables are (azimuth angle, hour of the day, day of the year)
#' @usage circular_variable_difference(value1,value2,values_in_full_circle)
#' @param value1 value which is compared against the other value
#' @param value2 value which the first value is compared against
#' @param values_in_full_circle maximum value for value1 and value2
#' @details Function call (circular_variable_difference(10,350,360)) gives 20 degrees as the result: 10 degrees is 20 degrees more compared to 350 degrees, not 340 degrees less.
#' @return smallest difference for the value1 compared against value2
circular_variable_difference <- function(direction1, direction2, digit_base) {
  difference <- direction1 - direction2
  difference[difference > (digit_base/2)] <- difference[difference > (digit_base/2)] - digit_base
  difference[difference < (-(digit_base/2))] <- digit_base + difference[difference < (-(digit_base/2))]
  invisible(difference)
}





#' Small helper function to remove selected variables in a workspace.
#'
#' @description This function removes variables in the global workspace, if they are present in it. If they are not, the function does not return error.
#' @usage ifrm("variable")
#' @param x The variable which is removed
ifrm <- function(x, env = globalenv())
{
  if(exists(x, envir = env))
  {
    rm(list = x, envir = env)
  }
}

# # Very useful functions of Johanna Piipponen
# source("functions_Johanna.R")
