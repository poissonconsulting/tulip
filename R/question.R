#' Yes or No
#'
#' Asks user a question and returns a TRUE or FALSE to indicate
#' whether user answered yes or no.
#' Modified from devtools package internal function so that yes returns TRUE.
#'
#' @param question a character scalar of the question to ask the user
#' @return A logical scalar indicating whether reponse was yes (TRUE) or no (FALSE).
#' @export
yesno <- function(question) {
  yeses <- c("Yes", "Yup", "Yeah")
  nos <- c("No", "No way", "Nope")

  cat(question)
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  utils::menu(qs[rand]) == which(rand == 1)
}
