\name{hlist_class}
\alias{hlist_class}
\title{Create a subclass of \code{HList}}
\usage{
  hlist_class(Class, classtype = "ANY", empty_names = TRUE,
    unique_names = FALSE, where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{\code{character} string name of the new
  class that will extend \code{HList}.}

  \item{classtype}{\code{character} The name of the class
  which all elements must inherit from.}

  \item{empty_names}{\code{logical} If \code{FALSE}, then
  no names can have values \code{""} or \code{NA}.}

  \item{unique_names}{\code{logical} If \code{TRUE}, then
  names must be unique.}

  \item{where}{Passed to \code{\link{setClass}}. The
  environment in which to store the definition.}
}
\value{
  \code{function} with usage \code{function(...)}, which
  will create objects of the newly defined class.
}
\description{
  Create a subclass of \code{\linkS4class{HList}}, which
  restricts elements of the list to be a specified class.
}
\examples{
NumericList <-
  hlist_class("NumericList", "numeric")
# creates a new class "NumericList"
showClass("NumericList")
# Create a new object of class NumericList
foo <- NumericList(a=1, b=2)
print(foo)
foo[["c"]] <- 3
print(foo)
# error
try(foo[["c"]] <- 3)
}
\seealso{
  \code{"\linkS4class{FunctionList}"} for a subclass
  created with this function used in this section.
  \code{\linkS4class{HList}}.
}

