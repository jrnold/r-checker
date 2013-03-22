\docType{class}
\name{CheckedFrame}
\alias{$<-,CheckedFrame-method}
\alias{CheckedFrame}
\alias{CheckedFrame-class}
\alias{[,CheckedFrame,ANY,ANY-method}
\alias{[,CheckedFrame,ANY,missing-method}
\alias{[,CheckedFrame,missing,ANY-method}
\alias{[,CheckedFrame,missing,missing-method}
\alias{[<-,CheckedFrame,ANY,ANY-method}
\alias{[<-,CheckedFrame,ANY,missing-method}
\alias{[<-,CheckedFrame,missing,ANY-method}
\alias{[<-,CheckedFrame,missing,missing-method}
\alias{[[<-,CheckedFrame,ANY,ANY-method}
\alias{[[<-,CheckedFrame,ANY,missing-method}
\alias{cbind2,CheckedFrame,ANY-method}
\alias{colnames<-,CheckedFrame-method}
\alias{dimnames<-,CheckedFrame,list-method}
\alias{initialize,CheckedFrame-method}
\alias{names<-,CheckedFrame,ANY-method}
\alias{rbind2,CheckedFrame,ANY-method}
\alias{rownames<-,CheckedFrame,ANY-method}
\alias{rownames<-,CheckedFrame,NULL-method}
\alias{show,CheckedFrame-method}
\title{Class \code{CheckedFrame}}
\usage{
  CheckedFrame(...)
}
\arguments{
  \item{...}{Data to include in the object.}
}
\description{
  Creates a new object directly extended
  \code{\link{data.frame}}, but with constrains that
  require columns. This class can be used to ensure that
  data frames have a specific structure.
}
\section{Slots}{
  \describe{ \item{\code{.Data}:}{Object of class
  \code{"list"}} \item{\code{checks}}{Object of class
  \code{list} containing \code{function} elements.  Each
  function in the list should take one argument, and return
  \code{logical}.} \item{\code{names}:}{Object of class
  \code{"character"} Column names}
  \item{\code{row.names}:}{Object of class
  \code{"data.frameRowLabels"} Row names}
  \item{\code{.S3Class}:}{Object of class
  \code{"character"} Name of \code{S3Class}} }
}

\section{Methods}{
  Methods commonly used with data frames are defined to
  return \code{"CheckedFrame"} objects where appropriate,
  or throw errors if they would create an invalid
  \code{"CheckedFrame"} object.

  \describe{ \item{[<-}{\code{signature(x =
  "CheckedFrame")}: } \item{[}{\code{signature(object =
  "CheckedFrame")}: Returns a
  \code{"\linkS4class{CheckedFrame}"} object if the
  returned object is valid, otherwise returns a
  \code{data.frame}.} \item{[[<-}{\code{signature(x =
  "CheckedFrame")}: } \item{$<-}{\code{signature(x =
  "CheckedFrame")}: } \item{cbind2}{\code{signature(x =
  "CheckedFrame")}: Use this instead of \code{cinbd}.}
  \item{colnames<-}{\code{signature(object =
  "CheckedFrame")}: }
  \item{dimnames<-}{\code{signature(object =
  "CheckedFrame")}: } \item{names<-}{\code{signature(x =
  "CheckedFrame")}: } \item{rbind2}{\code{signature(x =
  "CheckedFrame")}: Use this instead of \code{rbind}.}
  \item{rownames<-}{\code{signature(object =
  "CheckedFrame")}: } }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{Directly.} }
}
\examples{
checks <- TableChecks(columns =
                      ColumnCheckList(a = ColumnChecks(classtype="numeric",
                                        constraints =
                                        FunctionList(function(x) x > 0)),
                                      b = ColumnChecks(classtype="ANY"),
                                      c = ColumnChecks(classtype="factor")))
foo <-
  CheckedFrame(data.frame(a = runif(3), b = runif(3), c = letters[1:3]),
               checks = checks)
# works just like a normal data.frame
show(foo)
summary(foo)
# errors
try(foo$a <- as.character(foo$a))
try(foo["a", 1] <- -1)
try(foo$a <- NULL)
# errors
try(foo$b <- as.character(foo$b))
try(foo$d <- runif(3))
}
\keyword{classes}

