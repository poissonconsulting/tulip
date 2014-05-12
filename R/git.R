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

#' Commit Git Changes
#' 
#' Commits changes to the git repository in directory \code{dir}.
#' 
#' @param message string of commit message 
#' @param dir string of path to git repository directory
#' @return Invisible logical scalar indicating whether successful or error.
#' @export
git_commit <- function(message = paste(user(), Sys.time(), sep = ": "), dir = ".") {

  assert_that(is.string(message))
  assert_that(is.string(dir)) 
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  system("git add --all .")
  system(paste0("git commit -a -m \"", message, "\""))
  
  invisible(TRUE)
}

#' Push Git Commits
#' 
#' Pushes commits in git repository in directory \code{dir} to remote.
#' 
#' @param dir string of path to git repository directory
#' @return Invisible logical scalar indicating whether successful or error.
#' @export
git_push <- function (dir = ".") {
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  system("git push")
  invisible(TRUE)
}

#' Test If Git Branch
#' 
#' Test whether branch is a git branch
#' 
#' @param branch string of branch name 
#' @param dir string of path to git repository directory
#' @return Flag indicating whether or not branch is a branch
#' @export
is_git_branch <- function (branch = "master", dir = ".") {
  assert_that(is.string(branch))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  branches <- system("git branch", intern = TRUE)
  
  paste0("  ", branch) %in% branches || paste0("* ", branch) %in% branches
}

#' Test If Current Git Branch
#' 
#' Test whether branch is current git branch
#' 
#' @param branch string of branch name 
#' @param dir string of path to git repository directory
#' @return Flag indicating whether or not branch is the current branch
#' @export
is_git_current_branch <- function (branch = "master", dir = ".") {
  assert_that(is.string(branch))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  branches <- system("git branch", intern = TRUE)
  
  paste0("* ", branch) %in% branches
}

#' Change Git Branch
#' 
#' Changes git branch
#' 
#' @param branch string of branch name 
#' @param dir string of path to git repository directory
#' @return Invisible flag indicating whether successful or error.
#' @export
git_checkout <- function(branch = "master", dir = ".") {
  assert_that(is.string(branch))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
    
  if(is_git_current_branch(branch)) {
    message("branch '", branch, "' is already the current branch")
    return (invisible(TRUE))
  }
  
  if(!is_git_branch(branch)) {
    stop("branch '", branch,"' not found")
  }
  
  system(paste("git checkout", branch))
  return (invisible(TRUE))    
}

#' Merge Git Branches
#' 
#' Merges git branch with current branch
#' 
#' @param branch string of branch name 
#' @param dir string of path to git repository directory
#' @return Invisible flag indicating whether successful or error.
#' @export
git_merge <- function(branch = "dev", dir = ".") { 
  
  assert_that(is.string(branch))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  if(is_git_current_branch(branch)) {
    message("branch '", branch, "' is the current branch")
    return (invisible(TRUE))
  }
  
  if(!is_git_branch(branch)) {
    warning("branch '", branch, "' not found")
    return (invisible(TRUE))
  }
  
  system(paste("git merge", branch))
  return (invisible(TRUE))  
}

#' Create Git Branch
#' 
#' Create git branch
#' 
#' @param branch string of branch name 
#' @param dir string of path to git repository directory
#' @return Invisible flag indicating whether successful or error.
#' @export
git_branch <- function(branch = "dev", dir = ".") {

  assert_that(is.string(branch))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  if(is_git_branch(branch)) {
    stop("branch '", branch, "' already exists")
  }
  
  system(paste("git branch", branch))
  return (invisible(TRUE))
}

#' Push Origin of Git Branch
#' 
#' Pushes origin of git branch
#' 
#' @param branch string of branch name 
#' @param dir string of path to git repository directory
#' @return Invisible flag indicating whether successful or error.
#' @export
git_push_origin_branch <- function(branch = "dev", dir = ".") {
  
  assert_that(is.string(branch))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  if(!is_git_branch(branch)) {
    stop("branch '", branch, "' not found")
  }
  
  system(paste("git push -u origin", branch))
  return (invisible(TRUE))
}
