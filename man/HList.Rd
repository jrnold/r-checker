\docType{class}
\name{HList}
\alias{$<-,HList-method}
\alias{HList}
\alias{HList-class}
\alias{[,HList,ANY,ANY-method}
\alias{[,HList,missing,ANY-method}
\alias{[<-,HList,ANY,ANY-method}
\alias{[<-,HList,missing,ANY-method}
\alias{[[<-,HList,ANY,ANY-method}
\alias{[[<-,HList,missing,ANY-method}
\alias{c,HList-method}
\alias{length<-,HList,numeric-method}
\alias{names<-,HList,NULL-method}
\alias{show,HList-method}
\title{Homogenous lists}
\usage{
  HList(...)
}
\arguments{
  \item{...}{Passed onto generic functions.}
}
\description{
  An S4 subclass of \code{list} in which all elements of
  the list to be the same class. This is similar to the
  'atomic lists' in R in that all elements of the vector
  must be the same class, but the \code{HList} supports
  arbitrary classes. \code{NULL} values are also valid.
}
\section{Slots}{
  \describe{ \item{\code{.Data}}{Object of class
  \code{list}.} \item{\code{classtype}}{Object of class
  \code{character}. Required classtype for all elements in
  the list.} }
}

\section{Extends}{
  \describe{ \item{\code{list}}{Directly.} }
}

\section{Methods}{
  \describe{ \item{[<-}{\code{signature(x = "HList")}: ...
  } \item{[}{\code{signature(x = "HList")}: ... }
  \item{[[<-}{\code{signature(x = "HList")}: ... }
  \item{c}{\code{signature(x = "HList")}: ... }
  \item{show}{\code{signature(object = "HList")}: ... } }
}
\examples{
# This is valid with lists
foo <- list(a=1:10, b=c(1, 5))
foo[["c"]] <- c("a", "z")
# # But would not be valid with a homogenous list
foo <- HList(list(a=1:10, b=c(1, 5)), "numeric")
try(foo[["c"]] <- c("a", "z"))
foo[["c"]] <- c(1, 4, 10)
}
\seealso{
  \code{\link{hlist_class}},
}
\keyword{classes}

