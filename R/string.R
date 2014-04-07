capitalize_first_letter_words <- function (x) {
  assert_that(is.character(x))
  gsub(pattern = "\\b([a-z])", replacement = "\\U\\1", x, perl = TRUE)
}
