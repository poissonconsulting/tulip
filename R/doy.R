#' Day of Year
#'
#' Calculates day of the year
#' 
#' @param date object to convert - coerced to class Date.
#' @return An integer vector indicating the day of the year from 
#' 1 - 366.
#' @seealso \code{\link{dayte}}
#' @importFrom lubridate yday
#' @export
doy <- function(date) {
  warning("'doy' is depreciated - use 'lubridate::yday'")
  yday(date)
} 
