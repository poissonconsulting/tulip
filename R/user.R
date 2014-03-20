#' Get user
#' 
#' Gets current user as defined by operating system.
#' 
#' @return A character scalar
#' @export
get_user <- function () {
  user <- Sys.info()["user"]
  names(user) <- NULL
  user
}

#' Get home directory
#' 
#' Gets current user's home directory as defined by the operating system.
#' 
#' @return A character scalar
#' @export
get_home_dir <- function () {
  path.expand("~")
}
