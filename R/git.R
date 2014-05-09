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
#' @param message string of commit message 
#' @param push flag indicating whether to push to remote
#' @param dir string of path to git repository directory
#' @return Invisible logical scalar indicating whether successful or error.
#' @export
git_commit <- function(message = paste(user(), Sys.time(), sep = ": "), push = TRUE, dir = ".") {
  
  assert_that(is.string(message))
  assert_that(is.flag(push) && noNA(push))
  assert_that(is.string(dir))
  
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

#' Change Git Branch
#' 
#' Changes git branch
#' 
#' @param branch string of branch name 
#' @param create flag indicating whether to create branch
#' @param push flag indicating whether to push newly created branch to remote
#' @param dir string of path to git repository directory
#' @return Invisible flag indicating whether successful or error.
#' @export
git_branch <- function(branch = "master", create = TRUE, push = create, dir = ".") {
  assert_that(is.string(branch))
  assert_that(is.flag(create) && noNA(create))
  assert_that(is.flag(push) && noNA(push))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  branches <- system("git branch", intern = TRUE)
  
  if(paste0("* ", branch) %in% branches) {
    return (invisible(TRUE))
  }
  
  if(paste0("  ", branch) %in% branches) {
    system(paste("git checkout", branch))
    return (invisible(TRUE))    
  }
  if(!create) {
    warning("branch '", branch, "' not found")
    return (invisible(FALSE))        
  }
  system(paste("git branch", branch))
  
  if(push) {
    system(paste("git push -u origin", branch))
  }
  system(paste("git checkout", branch))
  return (invisible(TRUE))
}
