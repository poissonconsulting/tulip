\name{copy_directory}
\alias{copy_directory}
\title{Copy directory}
\usage{
copy_directory(dir, parent_dir = tempdir(), copy.date = TRUE,
  check = TRUE)
}
\arguments{
  \item{dir}{string of the directory to copy}

  \item{parent_dir}{string of the directory to move it to}

  \item{copy.date}{flag of whether to preserve file dates}

  \item{check}{a logical scalar indicating whether to
  confirm with user}
}
\value{
an invisible logical scalar indicating whether successful.
}
\description{
Copies directory specified by dir into parent_dir
}

