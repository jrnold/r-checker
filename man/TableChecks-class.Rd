\docType{class}
\name{TableChecks}
\alias{TableChecks}
\alias{TableChecks-class}
\title{Class \code{ColumnChecks}}
\usage{
  TableChecks(columns = ColumnCheckList(), ...)
}
\arguments{
  \item{columns}{\code{\linkS4class{ColumnCheckList}}
  Column level checks}

  \item{...}{Data to include in the new object. Named
  arguments correspond to slots in the class definition.}
}
\description{
  An object containing constraint checks intended to be
  used on a \code{data.frame}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{TableChecks(...)}.
}

\section{Slots}{
  \describe{
  \item{\code{columns}:}{\code{"ColumnCheckList"}.
  Column-level constraints.}
  \item{\code{exclusive}:}{\code{"logical"}. If
  \code{TRUE}, then the \code{data.frame} can only contain
  the columns in \code{columns}. If \code{FALSE}, the
  \code{data.frame} still must contain the columns in
  \code{columns}, but can contain additional columns.}
  \item{\code{ordered}:}{\code{"logical"}. If \code{TRUE},
  the columns in the \code{data.frame} must appear in the
  same order as they are listed in \code{columns}, and
  those columns must be the first columns in the
  \code{data.frame}. If \code{FALSE}, the the columns in
  \code{columns} can appear anywhere in the
  \code{data.frame}.}
  \item{\code{exclude}:}{\code{"character"}. A list of
  column names which cannot be in the data frame.}
  \item{\code{constraints}:}{\code{"FunctionList"}.
  Additional arbitrary constraints. Each function must
  return either \code{TRUE} if the constraint is satisfied,
  or \code{FALSE} if it is violated.} }
}
\examples{
showClass("TableChecks")
}
\seealso{
  Other Check.objects: \code{\link{ColumnCheckList}},
  \code{\link{ColumnCheckList}},
  \code{\link{ColumnChecks}}, \code{\link{ColumnChecks}}
}

