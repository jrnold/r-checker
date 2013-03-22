\docType{class}
\name{ColumnCheckList}
\alias{ColumnCheckList}
\alias{ColumnCheckList-class}
\title{Class \code{ColumnCheckList}}
\usage{
  ColumnCheckList(...)
}
\arguments{
  \item{...}{Objects of class
  \code{\linkS4class{ColumnChecks}} to include. All
  arguments must be named. The names of the
  \code{ColumnChecks} objects will correspond with their
  names in the \code{data.frame} to be checked.}
}
\description{
  An object containing multiple column-level constraint
  checks.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{ColumnCheckList(...)}.
}

\section{Extends}{
  \describe{ \item{\code{\linkS4class{HList}}}{directly.} }
}
\examples{
showClass("ColumnCheckList")
}
\seealso{
  Other Check.objects: \code{\link{ColumnChecks}},
  \code{\link{ColumnChecks}}, \code{\link{TableChecks}},
  \code{\link{TableChecks}}
}

