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
