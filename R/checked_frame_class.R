#' @include package.R
#' @include class-CheckedFrame.R
#' @export checked_frame_class
NULL

# checks: ColumnCheckList
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
#' @param constraints \code{TableChecks}. Contains the constraints that
#' will be used to check the validity of data frames in this class.
#' @return Invisibly returns a constructor function for the
#' new class.
checked_frame_class <- function(Class, columns=character(),
                                     exclusive=FALSE,
                                     constraints=list(),
                                     where=topenv(parent.frame())) {

  constraints <- do.call(FunctionList, constraints)
  
  setClass(Class, contains="CheckedFrame",
           prototype=
           prototype(x = new_data_frame(columns),
             
             columns = columns,
             exclusive = exclusive,
             constraints = constraints),
           where=where)
  
  setMethod("initialize", Class,
            function(.Object, x=new_data_frame(columns)) {
              callNextMethod(.Object, x,
                             constraints = constraints)
            }, where=where)

  setMethod("show", Class,
            function(object) {
              cat(sprintf("An object of class %s\n", dQuote(Class)))
              callGeneric(as(object, "CheckedFrame"))
            }, where=where)

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

  # colnames<-
  setMethod("colnames<-", "CheckedFrame",
            function(x, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)
  
  # rownames<-
  setMethod("rownames<-", c(x = "CheckedFrame"), 
            function(x, value) {
              callNextMethod()
            }, where=where)
  
  # names<-
  setMethod("names<-", "CheckedFrame",
            function(x, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)

  # names<-
  setMethod("dimnames<-", c(x="CheckedFrame", value="list"),
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
