\name{nlist}
\alias{nlist}
\title{Named List}
\usage{
  nlist(...)
}
\arguments{
  \item{...}{objects, possibly named.}
}
\value{
  An object of class \code{"namedList"}.
}
\description{
  Create a \code{"namedList"}, the S4 class equivalent to
  primitive lists.
}
\examples{
nlist(a=1, b="a", c=3L)
}

