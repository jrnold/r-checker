\docType{methods}
\name{check_constraints-methods}
\alias{check_constraints}
\alias{check_constraints,ANY,ColumnChecks-method}
\alias{check_constraints,CheckedFrame,missing-method}
\alias{check_constraints,data.frame,TableChecks-method}
\alias{check_constraints-methods}
\title{Methods for function \code{check_constraints}}
\arguments{
  \item{object}{The object to be checked.}

  \item{checks}{The object containing the constraints.}
}
\value{
  \code{TRUE} if all constraints are met. If a constraint
  is failed, a \code{character} vector describing the
  problems.
}
\description{
  Methods which check one object against another object
  containing constraints which must be met. Most commonly a
  \code{data.frame} is checked against constraints in a
  \code{\linkS4class{TableChecks}} object.
}
\section{Methods}{
  \describe{ \item{\code{signature(object = "ANY", checks =
  "ColumnChecks")}}{ } \item{\code{signature(object =
  "data.frame", checks = "TableChecks")}}{ }
  \item{\code{signature(object = "CheckedFrame", checks =
  "missing")}}{ } }
}
\examples{
# create a TableChecks object
# Require a numeric column foo and a factor column bar#
# Also require foo to be positive
positive <- function(x) x > 0
columns <- ColumnCheckList(foo = ColumnChecks(classtype = "numeric",
                             constraints = FunctionList(postive=positive)),
                           bar = ColumnChecks(classtype = "factor"))
# Require that the data.frame can contain ONLY foo and bar
checks <- TableChecks(columns = columns, exclusive = TRUE)
# this data frame is valid
check_constraints(data.frame(foo = 1:26, bar = letters),
                  checks)
# these are invalid
check_constraints(data.frame(foo = -(1:26), bar=letters), checks)
check_constraints(data.frame(foo = 1:26, bar=1:26), checks)
check_constraints(data.frame(foo = 1:26, bar=letters, baz=letters),
                  checks)
}

