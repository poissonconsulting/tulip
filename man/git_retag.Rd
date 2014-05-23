\name{git_retag}
\alias{git_retag}
\title{Retag Current Git}
\usage{
git_retag(tag, dir = ".")
}
\arguments{
  \item{tag}{string of tag name}

  \item{dir}{string of path to git repository directory}
}
\value{
Invisible logical scalar indicating whether successful or
error.
}
\description{
Retags current branch in git repository in directory
\code{dir}.
}

