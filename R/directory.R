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

#' Copy directory
#' 
#' Copies directory specified by dir into parent_dir
#' 
#' @param dir string of the directory to copy
#' @param parent_dir string of the directory to move it to
#' @param copy.date flag of whether to preserve file dates
#' @param check a logical scalar indicating whether to confirm with user
#' @return an invisible logical scalar indicating whether successful.
#' @export
copy_directory <- function (dir, parent_dir = tempdir(), copy.date = TRUE, 
  check = TRUE) {
  assert_that(is.string(dir))
  assert_that(is.string(parent_dir))
  assert_that(is.flag(copy.date) && noNA(copy.date))
  assert_that(is.flag(check) && noNA(check))
  
  if(!file.exists(dir)) {
    warning("directory '", dir, "' not found")
    return (invisible(TRUE))
  }
  
  if(check) {
    if(!yesno(paste0("Are you sure you want to copy the directory '", dir, "' to '", parent_dir, "'?"))) {
      return (invisible(FALSE))
    }
  }
  
  if(!file.exists(parent_dir))
    dir.create(parent_dir, recursive = TRUE)
    
  flag <- file.copy(from = dir, to = parent_dir, recursive = TRUE, copy.date = copy.date)    
  if(flag) {
    flag <- unlink(dir)
  } else
    warning("unable to copy directory")
  invisible(as.logical(flag))
}
