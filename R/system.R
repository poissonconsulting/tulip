#' Test if unix-based OS
#' 
#' Tests if running a unix-based operating system, i.e.,
#' not windows.
#'
#' @return A logical scalar
#' @export
is.unix <- function() {
  !is.windows()
}

#' Test if windows OS
#' 
#' Tests if running a windows operating system.
#' 
#' @return A logical scalar
#' @export
is.windows <- function() {
  flag <- Sys.info()["sysname"] == "Windows"
  names(flag) <- NULL
  flag
} 

#' Get user
#' 
#' Gets current user as defined by operating system.
#' 
#' @return A character scalar
#' @export
user <- function() {
  user <- Sys.info()["user"]
  names(user) <- NULL
  user
} 

