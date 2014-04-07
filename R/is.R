#' Test if named
#' 
#' @param x the object to test
#' @return A logical scalar indicating whether named
#' @export
is.named <- function (x) {
  is.character(names(x))
}
