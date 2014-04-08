#' Delete directory
#' 
#' Deletes directory specified by dir after possibly checking with user.
#' 
#' @param dir a character scalar of the directory to delete
#' @param check a logical scalar indicating whether to check with user
#' @return an invisible logical scalar indicating whether successful.
#' @export
delete_directory <- function (dir, check = TRUE) {
  assert_that(is.string(dir))
  assert_that(is.flag(check))
  
  if(!file.exists(dir)) {
    message("directory '", dir, "' does not exist")
    return(invisible(TRUE))
  }
  if(check) {
    if(!yesno(paste0("Are you sure you want to delete the directory '", dir, "'?"))) {
      return (invisible(FALSE))
    }
  }
  message("deleting dir '", dir, "' ...")
  invisible(unlink(dir, recursive = TRUE) == 0)
}
