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

#' Is Git Remote Repository
#' 
#' Test if directory is a git remote repository.
#' 
#' @param dir string of relative or absolute path to directory. Default is current working directory.
#' @return Flag indicating whether directory is a git repository.
#' @export
is_git_remote <- function (dir = ".") {
  assert_that(is.string(dir)) 
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  !identical(system("git remote -v", intern = TRUE), character(0))
}

#' Commit Git Changes
#' 
#' Commits changes to the git repository in directory \code{dir}.
#' 
#' @param message string of commit message 
#' @param push flag indicating whether to push commits
#' @param dir string of path to git repository directory
#' @return Invisible logical scalar indicating whether successful or error.
#' @export
git_commit <- function(message = paste(user(), Sys.time(), sep = ": "), 
  push = getOption("git.push", TRUE), dir = ".") {

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
    system("git push")
  
  invisible(TRUE)
}

#' Get Git Tags
#' 
#' Get Gits Tags
#' 
#' @param dir string of path to git repository directory
#' @return Character vector of tag names
#' @export
git_tags <- function (dir = ".") {
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  system("git tag", intern = TRUE)
}

#' Test If Git Tag
#' 
#' Test whether tag is a git tag
#' 
#' @param tag string of tag name 
#' @param dir string of path to git repository directory
#' @return Flag indicating whether or not tag is a tag
#' @export
is_git_tag <- function (tag, dir = ".") {
  assert_that(is.string(tag))
  
  tag %in% git_tags(dir)
}

#' Tag Current Git
#' 
#' Tags current branch in git repository in directory \code{dir}.
#' 
#' @param tag string of tag name 
#' @param retag tag indicating whether to retag if already exists
#' @param push flag indicating whether to push tag
#' @param dir string of path to git repository directory
#' @return Invisible logical scalar indicating whether successful or error.
#' @export
git_tag <- function (tag, retag = FALSE, push = getOption("git.push", TRUE), dir = ".") {
  assert_that(is.string(tag))
  assert_that(is.flag(retag) && noNA(retag))
  assert_that(is.flag(push) && noNA(push))
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  if(push & !is_git_remote(dir))
    warning("directory '", dir, "' is not a git remote repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  if(is_git_tag(tag)) {
    if(!retag)
      stop("tag '", tag, "' already exists")
    system(paste("git tag -d", tag)) 
    if(is_git_remote() && push)
      system(paste("git push --delete origin", tag))     
  }
  system(paste("git tag", tag))
  
  if(is_git_remote() && push)
    system(paste("git push origin", tag))
  
  invisible(TRUE)
}

#' Get Git Branches
#' 
#' Gets git branches
#' 
#' @param dir string of path to git repository directory
#' @return Character vector of branch names.
#' @export
git_branches <- function (dir = ".") {
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  branches <- system("git branch", intern = TRUE)
  
  gsub("^(([*] )|(  ))", "", branches)
}

#' Get Git Current Branch
#' 
#' Gets git current branch
#' 
#' @param dir string of path to git repository directory
#' @return String of current branch name.
#' @export
git_current_branch <- function (dir = ".") {
  assert_that(is.string(dir))
  
  if(!is_git_repository(dir))
    stop("directory '", dir, "' is not a git repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  branches <- system("git branch", intern = TRUE)
  
  branches <- branches[grepl("^[*]", branches)]
  gsub("^([*] )", "", branches)
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
  branch %in% git_branches(dir = dir)
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
  branch == git_current_branch(dir)
}

#' Change Git Branch
#' 
#' Change git branch
#' 
#' @param branch string of branch name 
#' @param create flag indicating whether to create branch if doesn't exist
#' @param push flag indicating whether to push origin of created branch
#' @param dir string of path to git repository directory
#' @return Invisible flag indicating whether successful or error.
#' @export
git_branch <- function(branch = "master", create = FALSE, push = getOption("git.push", TRUE), dir = ".") {
  
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
  
  if(!is_git_branch(branch)) {
    if(!create) {
      stop("branch '", branch, "' not found")
    }
    system(paste("git branch", branch))
    if(push)
      system(paste("git push -u origin", branch))
  }

  if(is_git_current_branch(branch)) {
    message("branch '", branch, "' is already the current branch")   
    return (invisible(TRUE))
  }
  system(paste("git checkout", branch))
  return (invisible(TRUE))
}

#' Push Origin of Current Git Branch
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



#' Switch remote URLs from HTTPS to SSH
#' 
#' Switches remote URLs from HTTPS to SSH
#' 
#' @param dir string of path to git repository directory
#' @seealso \url{https://help.github.com/articles/changing-a-remote-s-url}
#' @return Invisible flag indicating whether successful or error.
#' @export
git_url_http2ssh <- function (dir = ".") {
  assert_that(is.string(dir))
  
  if(!is_git_remote(dir))
    stop("directory '", dir, "' is not a git remote repository")
  
  check_git()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  
  txt <- system("git remote -v", intern = TRUE)[1]
    
  if(grepl("^origin\thttps://github[.]com/", txt)) {
      repository <- sub("(.*/)([^/]+)([.]git.*)", "\\2", txt, perl = TRUE)
      username <- sub("(.*/)([^/]+)(/)([^/]+)([.]git.*)", "\\2", txt, perl = TRUE)
    
      txt <- paste0("git remote set-url origin git@github.com:", 
        username, "/", repository, ".git")
    
    system(txt)
  }
  txt <- system("git remote -v", intern = TRUE)[1]
  invisible(grepl("^origin\tgit@github[.]com:", txt))
}
