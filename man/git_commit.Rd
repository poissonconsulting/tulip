\name{git_commit}
\alias{git_commit}
\title{Commit Git Changes}
\usage{
git_commit(message = paste(user(), Sys.time(), sep = ": "), push = TRUE,
  dir = ".")
}
\arguments{
  \item{message}{string of commit message}

  \item{push}{flag indicating whether to push to remote}

  \item{dir}{string of path to git repository directory}
}
\value{
Invisible logical scalar indicating whether successful or
error.
}
\description{
Commits (and potentially pushes) changes to the git
repository in directory \code{dir}.
}

