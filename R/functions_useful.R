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



#' Test if x not in y
#'
#' @description This is the opposite function to the \pkg{base} inline function \%in\%
#' @usage x \%notin\% y
#' @details \pkg{base} inline function \%in\% is in reality written as \%in\% <- function(x, table) match(x, table, nomatch = 0) > 0,
#' \%notin\% is written simply as \%notin\% <- function(x,y) !(x \%in\% y)
#' @param x object which is matched in y
#' @param y the list to what x is matched against
#' @return
#' A vector of the same length as x. A logical vector, indicating if a match was NOT located for each element of x: thus the values are TRUE or FALSE and never NA.
#' @export
`%notin%` <- function(x,y) !(x %in% y)



#' If x is null then return y
#'
#' @description This is an inline function that provides a default value in cases when the result is NULL
#' @usage x \%||\% y
#' @details The whole code of this function is `\%||\%` <- function(x, y) if (!is.null(x)) x else y \cr
#' Typical use for this inline function is to provide a default value if a function gives NULL, e.g. function_that_might_return_null() \%||\% default value
#' @param x Object which may return NULL (most likely a function)
#' @param y Default value in case of NULL
#' @return Either the value returned by x or default value y
#' @export
#' @seealso \url{http://adv-r.had.co.nz/Functions.html}
`%||%` <- function(x, y) if (!is.null(x)) x else y



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


# # Very useful functions of Johanna Piipponen
# source("functions_Johanna.R")
