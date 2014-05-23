\name{git_branch}
\alias{git_branch}
\title{Change Git Branch}
\usage{
git_branch(branch = "dev", create = FALSE, push = getOption("git.push",
  TRUE), dir = ".")
}
\arguments{
  \item{branch}{string of branch name}

  \item{create}{flag indicating whether to create branch if
  doesn't exist}

  \item{push}{flag indicating whether to push origin of
  created branch}

  \item{dir}{string of path to git repository directory}
}
\value{
Invisible flag indicating whether successful or error.
}
\description{
Change git branch
}

