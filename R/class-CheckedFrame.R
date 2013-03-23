#' @include package.R
#' @include hlist_class.R
#' @include class-TableChecks.R
#' @export CheckedFrame
#' @exportClass CheckedFrame
#' @exportMethod [<-
#' @exportMethod [[<-
#' @exportMethod $<-
#' @exportMethod [
#' @exportMethod cbind2
#' @exportMethod rbind2
#' @exportMethod colnames<-
#' @exportMethod rownames<-
#' @exportMethod names<-
NULL

#' @docType class
#' @keywords classes
#' @aliases CheckedFrame-class
#' @aliases CheckedFrame
#' @aliases [,CheckedFrame,missing,missing-method
#' @aliases [,CheckedFrame,missing,ANY-method
#' @aliases [,CheckedFrame,ANY,missing-method
#' @aliases [,CheckedFrame,ANY,ANY-method
#' @aliases [<-,CheckedFrame,ANY,ANY-method
#' @aliases [<-,CheckedFrame,ANY,missing-method
#' @aliases [<-,CheckedFrame,missing,ANY-method
#' @aliases [<-,CheckedFrame,missing,missing-method
#' @aliases [[<-,CheckedFrame,ANY,missing-method
#' @aliases [[<-,CheckedFrame,ANY,ANY-method
#' @aliases $<-,CheckedFrame-method
#' @aliases show,CheckedFrame-method
#' @aliases rbind2,CheckedFrame,ANY-method
#' @aliases cbind2,CheckedFrame,ANY-method
#' @aliases colnames<-,CheckedFrame-method
#' @aliases rownames<-,CheckedFrame,ANY-method
#' @aliases rownames<-,CheckedFrame,NULL-method
#' @aliases names<-,CheckedFrame,ANY-method
#' @aliases dimnames<-,CheckedFrame,list-method
#' @aliases initialize,CheckedFrame-method
#'
#' @title Class \code{CheckedFrame}
#'
#' @description Creates a new object directly extended \code{\link{data.frame}},
#' but with constrains that require columns. This class can be used
#' to ensure that data frames have a specific structure.
#'
#' @param ... Data to include in the object.
#' 
#' @section Slots:
#' 
#' \describe{
#' \item{\code{.Data}:}{Object of class \code{"list"}}
#' \item{\code{checks}}{Object of class \code{list} containing \code{function}
#' elements.  Each function in the list should take one argument, and return \code{logical}.}
#' \item{\code{names}:}{Object of class \code{"character"} Column names}
#' \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} Row names}
#' \item{\code{.S3Class}:}{Object of class \code{"character"} Name of \code{S3Class}}
#' }
#'
#' @section Methods:
#'
#' Methods commonly used with data frames are defined to return \code{"CheckedFrame"}
#' objects where appropriate, or throw errors if they would create an invalid
#' \code{"CheckedFrame"} object.
#'
#' \describe{
#'   \item{[<-}{\code{signature(x = "CheckedFrame")}: }
#'   \item{[}{\code{signature(object = "CheckedFrame")}:
#'   Returns a \code{"\linkS4class{CheckedFrame}"} object if the returned object is valid,
#'   otherwise returns a \code{data.frame}.}
#'   \item{[[<-}{\code{signature(x = "CheckedFrame")}: }
#'   \item{$<-}{\code{signature(x = "CheckedFrame")}: }
#'   \item{cbind2}{\code{signature(x = "CheckedFrame")}: Use this instead of \code{cinbd}.}
#'   \item{colnames<-}{\code{signature(object = "CheckedFrame")}: }
#'   \item{dimnames<-}{\code{signature(object = "CheckedFrame")}: }
#'   \item{names<-}{\code{signature(x = "CheckedFrame")}: }
#'   \item{rbind2}{\code{signature(x = "CheckedFrame")}: Use this instead of \code{rbind}.}
#'   \item{rownames<-}{\code{signature(object = "CheckedFrame")}: }
#' }
#'
#' @section Extends:
#'
#' \describe{
#' \item{\code{data.frame}}{Directly.}
#' }
#' 
#' @examples
#' checks <- TableChecks(columns =
#'                       ColumnCheckList(a = ColumnChecks(classtype="numeric",
#'                                         constraints =
#'                                         FunctionList(function(x) x > 0)),
#'                                       b = ColumnChecks(classtype="ANY"),
#'                                       c = ColumnChecks(classtype="factor")))
#' foo <- 
#'   CheckedFrame(data.frame(a = runif(3), b = runif(3), c = letters[1:3]),
#'                checks = checks)
#' # works just like a normal data.frame
#' show(foo)
#' summary(foo)
#' # errors
#' try(foo$a <- as.character(foo$a))
#' try(foo["a", 1] <- -1)
#' try(foo$a <- NULL)
#' # errors
#' try(foo$b <- as.character(foo$b))
#' try(foo$d <- runif(3))
CheckedFrame <-
  setClass("CheckedFrame", contains="data.frame",
           representation(checks = "TableChecks"),
           prototype(data.frame(),
                     checks = TableChecks()))

setValidity("CheckedFrame",
            function(object) {
              check_constraints(object)
            })

setMethod("show", "CheckedFrame",
          function(object) {
            cat(sprintf("An object of class %s\n", dQuote(class(object))))
            print(as(object, "data.frame"))
          })

###Methods

# [-method
setMethod("[", c(x="CheckedFrame", i="missing", j="missing", drop="ANY"),
          function(x, i, j, drop=TRUE) {
            if (drop && ncol(x) == 1) {
              x[[1]]
            } else {
              x
            }
          })

setMethod("[", c(x="CheckedFrame", i = "missing", j = "ANY", drop="ANY"), 
          function(x, i, j, drop=TRUE) {
            y <- data.frame(x)[ , j, drop=drop]
            tryCatch(CheckedFrame(y, checks=x@checks),
                     error = function(e) y)
          })

setMethod("[", c(x="CheckedFrame", i = "ANY", j = "missing", drop="ANY"),
          function(x, i, j, drop = TRUE) {
            y <- as(x, "data.frame")[i, , drop=drop]
            tryCatch(CheckedFrame(y, checks=x@checks),
                     error = function(e) y)
          })

setMethod("[", c(x="CheckedFrame", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, drop = TRUE) {
            y <- as(x, "data.frame")[i, j, drop=drop]
            tryCatch(CheckedFrame(y, checks=x@checks),
                     error = function(e) y)
          })


# [<- method

setMethod("[<-", c(x="CheckedFrame", i="missing", j="missing"),
          function(x, i, j, value) {
            y <- callGeneric(data.frame(x), , ,value=value)
            CheckedFrame(y, checks=x@checks)
          })

setMethod("[<-", c(x="CheckedFrame", i="missing", j="ANY"),
          function(x, i, j, value) {
            y <- callGeneric(data.frame(x), , j,value=value)
            CheckedFrame(y, checks=x@checks)
          })

setMethod("[<-", c(x="CheckedFrame", i="ANY", j="missing"),
          function(x, i, j, value) {
            y <- callGeneric(data.frame(x), i, ,value=value)
            CheckedFrame(y, checks=x@checks)
          })

setMethod("[<-", c(x="CheckedFrame", i="ANY", j="ANY"),
          function(x, i, j, value) {
            y <- callGeneric(data.frame(x), i, j,value=value)
            CheckedFrame(y, checks=x@checks)
          })


# [[<- method

setMethod("[[<-", c(x="CheckedFrame", i="ANY", j="missing", value="ANY"),
          function(x, i, j, value) {
            y <- callGeneric(data.frame(x), i, value=value)
            CheckedFrame(y, checks=x@checks)
          })

setMethod("[[<-", c(x="CheckedFrame", i="ANY", j="ANY", value="ANY"),
          function(x, i, j, value) {
            y <- callGeneric(data.frame(x), i, j, value=value)
            CheckedFrame(y, checks=x@checks)
          })

# $<- method
setMethod("$<-", "CheckedFrame",
          function(x, name, value) {
            y <- callNextMethod()
            CheckedFrame(y, checks=x@checks)
          })

# rbind2 method
setMethod("rbind2", "CheckedFrame",
          function(x, y, ...) {
            z <- rbind(as(x, "data.frame"), as(y, "data.frame"), ...)
            CheckedFrame(z, checks=x@checks)
          })

# cbind2 method
setMethod("cbind2", "CheckedFrame",
          function(x, y, ...) {
            z <- cbind(as(x, "data.frame"), as(y, "data.frame"), ...)
            CheckedFrame(z, checks=x@checks)
          })

# names<-
setMethod("names<-", "CheckedFrame",
          function(x, value) {
            y <- callNextMethod()
            validObject(y)
            y
          })


# colnames<-
setMethod("colnames<-", "CheckedFrame",
          function(x, value) {
            names(x) <- value
            x
          })

# rownames<-
setMethod("rownames<-", c(x = "CheckedFrame", value = "ANY"),
          function(x, value) {
            y <- callNextMethod()
            validObject(y)
            y
          })

setMethod("rownames<-", c(x = "CheckedFrame", value = "NULL"),
          function(x, value) {
            x@row.names <- seq_len(nrow(x))
            validObject(x)
            x
          })


# names<-
setMethod("dimnames<-", c(x="CheckedFrame", value="list"),
          function(x, value) {
            rownames(x) <- value[[1]]
            colnames(x) <- value[[2]]
            x
          })

# coerce back to the S3 data.frame
# if this is not done, the new object will keep the old slots.
# it will also give warnings when altered that the output is no
# longer an S4 object.
setAs("CheckedFrame", "data.frame",
      function(from) {
        data.frame(from)
      })

