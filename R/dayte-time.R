#' @title Daytetime
#'
#' @description
#' Converts object dt to a daytetime.
#' 
#' @param dt object to convert
#' @param year an integer element indicating 
#' the year of the dayte. By default = 2000.
#' @param month an 
#' integer element indicating the month to split the dayte by.
#' @return A daytetime object.
#' @seealso \code{\link{dayte}}.
#' @importFrom lubridate year month day hour minute second with_tz
#' @export
dayte_time <- function(dt, year = 2000, month = 1) {
  UseMethod("dayte_time", dt)
}

#' @method dayte_time integer
#' @export
dayte_time.integer <- function(dt, year = 2000, month = 1) {
  if (length(month) != 1) 
    stop("month must be an integer of length 1")
  if (!month %in% 1:12) 
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(as.Date(paste(year, "01", "01", sep = "-")), tz = "UTC")
  x <- x + dt
  x <- with_tz(x, tzone = "UTC")
  
  if (month == 1) 
    return(x)
  
  yr <- year(x)
  bol <- month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return(as.POSIXct(as.Date(paste(yr, format(x, format = "%m-%d"), sep = "-")), 
    tz = "UTC"))
}

#' @method dayte_time numeric
#' @export
dayte_time.numeric <- function(dt, year = 2000, month = 1) {
  if (length(month) != 1) 
    stop("month must be an integer of length 1")
  if (!month %in% 1:12) 
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(as.Date(paste(year, "01", "01", sep = "-")), tz = "UTC")
  x <- x + dt
  x <- with_tz(x, tzone = "UTC")
  
  if (month == 1) 
    return(x)
  
  yr <- year(x)
  bol <- month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return(as.POSIXct(as.Date(paste(yr, format(x, format = "%m-%d"), sep = "-")), 
    tz = "UTC"))
}

#' @method dayte_time Date
#' @export
dayte_time.Date <- function(dt, year = 2000, month = 1) {
  if (length(month) != 1) 
    stop("month must be an integer of length 1")
  if (!month %in% 1:12) 
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(as.Date(paste(year, format(dt, format = "%m-%d"), sep = "-")), 
    tz = "UTC")
  
  if (month == 1) 
    return(x)
  
  yr <- year(x)
  bol <- month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return(as.POSIXct(as.Date(paste(yr, format(dt, format = "%m-%d"), sep = "-")), 
    tz = "UTC"))
}

#' @method dayte_time POSIXct
#' @export
dayte_time.POSIXct <- function(dt, year = 2000, month = 1) {
  if (length(month) != 1) 
    stop("month must be an integer of length 1")
  if (!month %in% 1:12) 
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(paste(year, format(dt, format = "%m-%d %H:%M:%S"), sep = "-"), 
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  if (month == 1) 
    return(x)
  
  yr <- year(x)
  bol <- month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return(as.POSIXct(paste(yr, format(dt, format = "%m-%d %H:%M:%S"), sep = "-"), 
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}

#' @method dayte_time POSIXlt
#' @export
dayte_time.POSIXlt <- function(dt, year = 2000, month = 1) {
  if (length(month) != 1) 
    stop("month must be an integer of length 1")
  if (!month %in% 1:12) 
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(paste(year, format(dt, format = "%m-%d %H:%M:%S"), sep = "-"), 
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  if (month == 1) 
    return(x)
  
  yr <- year(x)
  bol <- month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return(as.POSIXct(paste(yr, format(dt, format = "%m-%d %H:%M:%S"), sep = "-"), 
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
} 
