#' @include package.R
#' @include class-CheckedFrame.R
#' @export checked_frame_class
NULL

# param checks ColumnCheckList
proto_data_frame <- function(checks) {
  .data <- data.frame()
  for (i in seq_along(checks)) {
    cname <- names(checks)[i]
    column <- checks[[i]]
    if (column@classtype == "ANY") {
      .data[[cname]] <- logical()
    } else {
      .data[[cname]] <- new(column@classtype)
   }
  }
  .data
}

#' Create subclasss of \code{CheckedFrame}
#'
#' This function creates a class which directly extends
#' \code{CheckedFrame}.
#'
#' @param Class \code{character} Name of the new class.
#' @param checks \code{TableChecks}. Contains the constraints that 
#' will be used to check the validity of data frames in this class.
#' @param where Passed to \code{\link{setClass}}. The environment
#' in which to store the definition.
#' @param ... Arguments overrides the slot values in \code{checks}.
#' @return Invisibly returns a constructor function for the
#' new class.
checked_frame_class <- function(Class,
                                checks = TableChecks(),
                                ..., 
                                where=topenv(parent.frame())) {

  ## Fill in values of checks from optional args
  optargs <- list(...)
  for (i in seq_along(optargs)) {
    slotname <- names(optargs)[i]
    slotval <- optargs[[i]]
    slot(checks, slotname) <- slotval
  }

  setClass(Class, contains="CheckedFrame", where=where)

  setMethod("initialize", Class,
           function(.Object, x) {
             callNextMethod(.Object, x, checks = checks)
           }, where = where)

  # [-method
  # callNextMethod does not work for [
  setMethod("[", c(x=Class, i="missing", j="missing"),
            function(x, i, j, drop=TRUE) {
              if (drop && ncol(x) == 1) {
                x[[1]]
              } else {
                x
              }
            }, where = where)

  setMethod("[", c(x=Class, i = "missing", j = "ANY"), 
            function(x, i, j, drop=TRUE) {
              y <- callGeneric(as(x, "CheckedFrame"), , j, drop=drop)
              if (is(y, "CheckedFrame")) {
                y <- new(Class, y)
              }
              y
            }, where = where)
  
  setMethod("[", c(x=Class, i = "ANY", j = "missing"), 
            function(x, i, j, drop=TRUE) {
              y <- callGeneric(as(x, "CheckedFrame"), i, , drop=drop)
              if (is(y, "CheckedFrame")) {
                y <- new(Class, y)
              }
              y
            }, where = where)

  setMethod("[", c(x=Class, i = "ANY", j = "ANY"), 
            function(x, i, j, drop=TRUE) {
              y <- callGeneric(as(x, "CheckedFrame"), i, j, drop=drop)
              if (is(y, "CheckedFrame")) {
                y <- new(Class, y)
              }
              y
            }, where = where)
  
  # [<- method
  # callNextMethod does not work for [<-
  setMethod("[<-", c(x=Class, i="missing", j="missing"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "CheckedFrame"), , , value=value)
              new(Class, y)
            }, where=where)
  
  setMethod("[<-", c(x=Class, i="missing", j="ANY"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "CheckedFrame"), , j, value=value)
              new(Class, y)
            }, where=where)
  
  setMethod("[<-", c(x=Class, i="ANY", j="missing"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "CheckedFrame"), i, , value=value)
              new(Class, y)
            }, where=where)
  
  setMethod("[<-", c(x=Class, i="ANY", j="ANY"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "CheckedFrame"), i, j, value=value)
              new(Class, y)
            }, where=where)

  # callNextMethod does not work for [[<-
  setMethod("[[<-", c(x=Class, i="ANY", j="missing", value="ANY"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "CheckedFrame"), i, , value=value)
              new(Class, y)
            }, where=where)
  
  setMethod("[[<-", c(x=Class, i="ANY", j="ANY", value="ANY"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "CheckedFrame"), i, j, value=value)
              new(Class, y)
          }, where=where)

  # callNextMethod does not work for $<-
  setMethod("$<-", c(x=Class),
            function(x, name, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)

  setMethod("rbind2", Class,
          function(x, y, ...) {
            z <- callNextMethod()
            new(Class, z)
          }, where=where)

  setMethod("cbind2", Class,
            function(x, y, ...) {
              z <- callNextMethod()
              new(Class, z)
            }, where=where)

  # names<-
  setMethod("names<-", Class,
            function(x, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)
  
  # colnames<-
  setMethod("colnames<-", Class,
            function(x, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)
  
  # rownames<-
  setMethod("rownames<-", c(x = Class),
            function(x, value) {
              callNextMethod()
            }, where=where)
  
  # names<-
  setMethod("dimnames<-", c(x=Class, value="list"),
            function(x, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)
  
  setAs("data.frame", Class,
        function(from, to) new(Class, from), where=where)
  
  .f <- function(...) {
    new(Class, data.frame(...))
  }
  invisible(.f)
}
