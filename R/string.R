#' Capitalize First Letter Words
#' 
#' Capitalizes first letter of words in character vector.
#' 
#' @param x character vector
#' @return Modified character vector
#' @export 
capitalize_first_letter_words <- function (x) {
  assert_that(is.character(x))
  gsub(pattern = "\\b([a-z])", replacement = "\\U\\1", x, perl = TRUE)
}

#' Reverse Strings
#' 
#' Reverses strings
#' 
#' @param x character vector
#' @return Reversed character vector
#' @examples
#' reverse_strings(c("abc","def"))
#' @export 
reverse_strings <- function(x) {
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}

#' Paste Names
#' 
#' Paste names
#' 
#' @param x character vector
#' @param quotes flag indicating whether to quote names
#' @return String of pasted names
#' @examples
#' paste_names(c("x","y","z"))
#' paste_names(c("x","y","z"), TRUE)
#' @export
paste_names <- function (x, quotes = FALSE) {
  assert_that(is.flag(quotes) && noNA(quotes))
  
  x <- paste0("'", paste0(x, collapse = "', '"), "'")
  x <- sub(",(?= '\\w+'$)", " and", x, perl = TRUE)
  x
}
