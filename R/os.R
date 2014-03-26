#' Test if OSX
#' 
#' Tests if running an OSX operating system.
#' 
#' @return A logical scalar
#' @export
is.osx <- function () {
  flag <- Sys.info()["sysname"] == "Darwin"
  names(flag) <- NULL
  flag
}

#' Test if unix-based OS
#' 
#' Tests if running a unix-based operating system, i.e.,
#' OSX or Linux.
#'
#' @return A logical scalar
#' @export
is.unix <- function () {
  flag <- Sys.info()["sysname"] != "Windows"
  names(flag) <- NULL
  flag
}

#' Test if windows
#' 
#' Tests if running a windows operating system.
#' 
#' @return A logical scalar
#' @export
is.windows <- function () {
  flag <- Sys.info()["sysname"] == "Windows"
  names(flag) <- NULL
  flag
}

#' Convert OSX file path to windows format
#' 
#' @return A character scalar of the modified path
#' @export
osx2win <- function (path) {
  assert_that(is.character(path))
  
  gsub("/", "\\\\", path)
}
