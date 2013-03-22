\docType{class}
\name{ColumnChecks}
\alias{ColumnChecks}
\alias{ColumnChecks-class}
\title{Class \code{ColumnChecks}}
\usage{
  ColumnChecks(classtype = "ANY", ...)
}
\arguments{
  \item{classtype}{Name of to class which the column must
  belong.}

  \item{...}{Data to be included in the object.}
}
\description{
  An object containing column-level checks. The following
  constraint checks are implemented:

  \itemize{ \item class type \item uniqueness \item missing
  values \item list of arbitrary constraints }
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{ColumnChecks(...)}.
}

\section{Slots}{
  \describe{ \item{\code{classtype}}{\code{character}.
  Class type of object to be checked.}
  \item{\code{missings}}{\code{logical}. If \code{FALSE},
  then no \code{NA} values are allowed in the object to be
  checked.} \item{\code{unique}}{\code{logical}. If
  \code{FALSE}, then duplicate values are allowed in the
  object to be checked.}
  \item{\code{constraints}:}{\code{"FunctionList"}.
  Additional arbitrary constraints. Each function must
  return either \code{TRUE} if the constraint is satisfied,
  or \code{FALSE} if it is violated. These functions can
  also return vectors, in which case, the constraint is
  violated if there is any \code{FALSE} value.} }
}
\examples{
showClass("ColumnChecks")
}
\seealso{
  Other Check.objects: \code{\link{ColumnCheckList}},
  \code{\link{ColumnCheckList}}, \code{\link{TableChecks}}
}

