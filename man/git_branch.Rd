\name{git_branch}
\alias{git_branch}
\title{Change Git Branch}
\usage{
git_branch(dir = ".", branch = "master", create = FALSE)
}
\arguments{
  \item{dir}{string of path to git repository directory}

  \item{branch}{string of branch name}

  \item{create}{flag indicating whether to create branch}

  \item{push}{}
}
\value{
Invisible flag indicating whether successful or error.
}
\description{
Changes git branch
}

