split_dayte <- function(dayte, year, month) {
  stopifnot(month %in% 1:12)
  if (month != 12) {
    bol <- month(dayte) > month
    if (month == 1) {
      dayte[!bol] <- as.Date(paste(year + 1, month(dayte), day(dayte), sep = "-"))[!bol]
    } else {
      dayte[bol] <- as.Date(paste(year - 1, month(dayte), day(dayte), sep = "-"))[bol]
    }
  }
  return(dayte)
}

#' @title Dayte
#'
#' @description
#' Converts object dt to a dayte.
#' 
#' @param dt object to convert
#' @param year an integer element indicating the year of the dayte. By default 
#'  = 2000.
#' @param month an integer element indicating the month to split the dayte by.
#' @return A dayte.
#' @seealso \code{\link{doy}} and \code{\link{dayte_time}}.
#' @export
dayte <- function(dt, year = 2000, month = 0) {
  UseMethod("dayte", dt)
}

#' @method dayte numeric
#' @export
dayte.numeric <- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste0(year - 1, "-12-31")) + dt
  dayte <- split_dayte(dayte, year, month)
  return(dayte)
}

#' @method dayte integer
#' @export
dayte.integer <- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste0(year - 1, "-12-31")) + dt
  dayte <- split_dayte(dayte, year, month)
  return(dayte)
}

#' @method dayte Date
#' @export
dayte.Date <- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste(year, format(dt, format = "%m-%d"), sep = "-"))
  dayte <- split_dayte(dayte, year, month)
  return(dayte)
}

#' @method dayte POSIXct
#' @export
dayte.POSIXct <- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste(year, format(dt, format = "%m-%d"), sep = "-"))
  dayte <- split_dayte(dayte, year, month)
  return(dayte)
}

#' @method dayte POSIXlt
#' @export
dayte.POSIXlt <- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste(year, format(dt, format = "%m-%d"), sep = "-"))
  dayte <- split_dayte(dayte, year, month)
  return(dayte)
} 
