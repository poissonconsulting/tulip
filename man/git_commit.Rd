\name{git_commit}
\alias{git_commit}
\title{Commit Git Changes}
\usage{
git_commit(dir = ".", push = TRUE, message = paste(user(), Sys.time(), sep
  = ": "))
}
\arguments{
  \item{dir}{string of path to git repository directory}

  \item{push}{flag indicating whether to push to remote}

  \item{message}{string of commit message}
}
\value{
Invisible logical scalar indicating whether successful or
error.
}
\description{
Commits (and potentially pushes) changes to the git
repository in directory \code{dir}.
}

