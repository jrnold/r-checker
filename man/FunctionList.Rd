\docType{class}
\name{FunctionList}
\alias{FunctionList}
\alias{FunctionList-class}
\title{List of Functions}
\usage{
  FunctionList(...)
}
\arguments{
  \item{...}{Objects of class \code{function}.}
}
\description{
  A class which is a list of functions. This is a subclass
  of \code{"\linkS4class{HList}"}, and was itself created
  using the function \code{\link{hlist_class}}.
}
\section{Extends}{
  \describe{ \item{\code{HList}}{directly} }
}

\section{Slots}{
  \describe{ \item{\code{.Data}:}{\code{"list"}}
  \item{\code{classtype}:}{\code{"character"} Always equal
  to \code{"function"} in this class.}
  \item{\code{names}:}{\code{"character"}} }
}
\examples{
flist <- FunctionList(sum = sum, mean=mean)
flist2 <- c(flist, list(max=max))
x <- rnorm(35)
lapply(flist2, function(f) f(x))
# cannot add non-functions to it
try(flist[["foo"]] <- 1)
}
\seealso{
  \code{\link{hlist_class}}, \code{\linkS4class{HList}},
}

