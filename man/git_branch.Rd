\name{git_branch}
\alias{git_branch}
\title{Change Git Branch}
\usage{
git_branch(branch = "master", create = TRUE, push = create, dir = ".")
}
\arguments{
  \item{branch}{string of branch name}

  \item{create}{flag indicating whether to create branch}

  \item{push}{flag indicating whether to push newly created
  branch to remote}

  \item{dir}{string of path to git repository directory}
}
\value{
Invisible flag indicating whether successful or error.
}
\description{
Changes git branch
}

