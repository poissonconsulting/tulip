#' @title Filename extension utilities
#' 
#' @description Replaces, adds or removes (rm) extensions from filenames or test whether
#' filename has an extension or is a filename with a particular extension. 
#' Functions are primarily wrappers
#' on the tools package functions \code{file_ext} and \code{file_path_sans_ext}.
#' 
#' @usage
#' get_ext(x)
#' has_ext(x)
#' is_ext(x, ext)
#' rm_ext(x)
#' replace_ext(x, ext)
#' add_ext(x, ext)
#' 
#' @param x a character vector of the filenames.
#' @param ext a character scalar of the extension with or without a leading point.
#' @return A character vector of the extensions or modified filenames or a logical vector
#' indicating whether a filename has an extension or is a particular extension.
#' @seealso \code{\link{file_ext}} and \code{\link{file_path_sans_ext}}
#' @aliases has_ext is_ext rm_ext replace_ext add_ext
#' @examples
#' 
#' x <- c("file", "file.txt", "file.txt.zip")
#' get_ext(x)
#' has_ext(x)
#' is_ext(x, "txt")
#' is_ext(x, ".txt")
#' rm_ext(x)
#' replace_ext(x, "md")
#' add_ext(x, "md")
#' @export
get_ext <- function (x) {
  file_ext(x)
}

rm_leading_dot <- function (x) {
  sub("^[.]", "", x)
}

#' @export
has_ext <- function (x) {
  get_ext(x) != ""
}

#' @export
is_ext <- function (x, ext) {
  assert_that(is.string(ext))
  ext <- rm_leading_dot (ext)
  get_ext(x) == ext
}

#' @export
rm_ext <- function (x) {
  file_path_sans_ext(x)
}

#' @export
replace_ext <- function (x, ext) {
  assert_that(is.string(ext))
  ext <- rm_leading_dot (ext)
  paste(rm_ext(x), ext, sep = ".")
}

#' @export
add_ext <- function (x, ext) {
  assert_that(is.string(ext))
  ext <- rm_leading_dot (ext)
  paste(x, ext, sep = ".")
}
