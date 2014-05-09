#' Is Git Repository
#' 
#' Test if directory is a git repository.
#' 
#' @param dir string of relative or absolute path to directory. Default is current working directory.
#' @return Flag indicating whether directory is a git repository.
#' @examples
#' is_git_repository()
#' @export
is_git_repository <- function (dir = ".") {
  if(!file.exists(dir))
    stop("directory '", dir, "' not found")
  file.exists(file.path(dir, ".git"))
}

#' Check Git
#' 
#' Checks if git binary is on path. Throws error if not.
#' 
#' @examples
#' check_git()
#' @importFrom devtools on_path
#' @export
check_git <- function () {
  if (!on_path("git")) 
    stop("git is not on path")
}

#' Commit Git Changes
#' 
#' Commits (and potentially pushes) changes to the git repository in directory \code{dir}.
#' 
#' @param dir string of path to git repository directory
#' @param push flag indicating whether to push to remote
#' @param message string of commit message 
#' @return Invisible logical scalar indicating whether successful or error.
#' @export
git_commit <- function(dir = ".", push = TRUE, message = paste(user(), Sys.time(), sep = ": ")) {
  
  assert_that(is.string(dir))
  assert_that(is.string(message))
  assert_that(is.flag(push) && noNA(push))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  system("git add --all .")
  system(paste0("git commit -a -m \"", message, "\""))
  
  if(push)
    system("git push -u origin HEAD")
  
  invisible(TRUE)
} 
