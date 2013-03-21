#' @include package.R
#' @include class-CheckedFrame.R
#' @export checked_frame_class
NULL

new_data_frame <- function(columns=character()) {
  .data <- data.frame()
  for (i in seq_along(columns)) {
    cname <- names(columns)[i]
    classname <- columns[i]
    if (classname == "ANY") {
      .data[[cname]] <- numeric()
    } else {
      .data[[cname]] <- new(classname)
   }
  }
  .data
}

#' Create subclasss of \code{CheckedFrame}
#'
#' This function creates a class which directly extends
#' \code{CheckedFrame} with the requirement that the slots
#' (\code{columns}, and \code{exclusive}
#' take specific values.
#'
#' @param Class \code{character} Name of the new class.
#' @param columns Named \code{character} vector. The names are
#' the names of required columns; the values are the classes
#' of those columns.  Use \code{ANY} to allow a class
#' to be anything.
#' @param exclusive \code{logical} If \code{TRUE}, then
#' the data frame can only contain the columns in \code{columns}.
#' @param constraints \code{list} of functions. Each function should
#' take only one argument, and return \code{logical}.
#' @param where \code{environment}. The environment in which to store
#' the definition. See \code{\link{setClass}}.
#' @return Invisibly returns a constructor function for the
#' new class.
#'
#' @examples
#' Foo <-
#'   checked_frame_class("Foo",
#'                          columns = c(a = "numeric", b = "ANY", c = "factor"),
#'                          constraints = list(function(x) {x$a > 0}))
#' showClass("Foo")
#' 
#' # Create a new "Foo" object
#' foo <- Foo(data.frame(a = runif(3), b = runif(3), c = letters[1:3]))
#' # this also works
#' # new("Foo", data.frame(a = runif(3), b = runif(3), c = letters[1:3]))
#' # works like a normal data.frame
#' print(foo)
#' summary(foo)
#' # errors
#' try(foo$a <- as.character(foo$a))
#' try(foo["a", 1] <- -1)
#' try(foo$a <- NULL)
#' # errors
#' try(foo$b <- as.character(foo$b))
#' try(foo$d <- runif(3))
checked_frame_class <- function(Class, columns=character(),
                                     exclusive=FALSE,
                                     constraints=list(),
                                     where=topenv(parent.frame())) {

  constraints <- FunctionList(constraints)
  
  setClass(Class, contains="CheckedFrame",
           prototype=
           prototype(x=new_data_frame(columns), columns=columns,
                     exclusive=exclusive,
                     constraints=constraints),
           where=where)
  
  setMethod("initialize", Class,
            function(.Object, x=new_data_frame(columns)) {
              callNextMethod(.Object, x=x,
                             columns=columns,
                             exclusive=exclusive,
                             constraints=constraints
                             )
            }, where=where)

  setMethod("show", Class,
            function(object) {
              cat(sprintf("An object of class %s\n", dQuote(Class)))
              callGeneric(as(object, "CheckedFrame"))
            }, where=where)

  # [-method
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