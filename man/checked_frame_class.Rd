\name{checked_frame_class}
\alias{checked_frame_class}
\title{Create subclasss of \code{CheckedFrame}}
\usage{
  checked_frame_class(Class, checks = checks(),
    where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{\code{character} Name of the new class.}

  \item{checks}{\code{TableChecks}. Contains the
  constraints that will be used to check the validity of
  data frames in this class.}

  \item{where}{Passed to \code{\link{setClass}}. The
  environment in which to store the definition.}
}
\value{
  Invisibly returns a constructor function for the new
  class.
}
\description{
  This function creates a class which directly extends
  \code{CheckedFrame}.
}

