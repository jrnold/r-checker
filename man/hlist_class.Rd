\name{hlist_class}
\alias{hlist_class}
\title{Create a subclass of \code{HList}}
\usage{
  hlist_class(Class, classtype = "ANY",
    where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{\code{character} string name of the new
  class that will extend \code{HList}.}

  \item{classtype}{\code{character} The name of the class
  which all elements must inherit from. This is tested with
  \code{is}.}

  \item{where}{Passed to \code{\link{setClass}}.}
}
\description{
  Creates a new subclass of \code{HList} for a specific
  class.
}
\examples{
NumericList <-
  hlist_class("NumericList", "numeric")
# creates a new class "NumericList"
showClass("NumericList")
# Create a new object of class NumericList
foo <- NumericList(list(a=1, b=2))
print(foo)
foo[["c"]] <- 3
print(foo)
# error
try(foo[["c"]] <- 3)
}

