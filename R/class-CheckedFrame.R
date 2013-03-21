## #' @include package.R
## #' @include hlist_class.R
## #' @export CheckedFrame
## #' @export validate_data_frame
## #' @exportClass CheckedFrame
## #' @exportMethod [<-
## #' @exportMethod [[<-
## #' @exportMethod $<-
## #' @exportMethod [
## #' @exportMethod cbind2
## #' @exportMethod rbind2
## #' @exportMethod colnames<-
## #' @exportMethod rownames<-
## #' @exportMethod names<-
## NULL

## #' Data Frame with constraints
## #'
## #' Creates a new object directly extended \code{\link{data.frame}},
## #' but with constrains that require columns. This class can be used
## #' to ensure that data frames have a specific structure.
## #'
## #' @param ... Data to include in the object.
## #' 
## #' @section Slots:
## #' 
## #' \describe{
## #' \item{\code{.Data}:}{Object of class \code{"list"}}
## #' \item{\code{columns}}{Named \code{character} vector. The names are the
## #' column names, and the values are the required classes of the column.}
## #' \item{\code{exclusive}}{Object of class \code{logical}. If \code{TRUE},
## #' then the data frame cannot contain any columns other than those
## #' in \code{columns}}
## #' \item{\code{constraints}}{Object of class \code{list} containing \code{function}
## #' elements.  Each function in the list should take one argument, and return \code{logical}.}
## #' \item{\code{names}:}{Object of class \code{"character"} Column names}
## #' \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} Row names}
## #' \item{\code{.S3Class}:}{Object of class \code{"character"} Name of \code{S3Class}}
## #' }
## #'
## #' @section Methods:
## #'
## #' Methods commonly used with data frames are defined to return \code{"CheckedFrame"}
## #' objects where appropriate, or throw errors if they would create an invalid
## #' \code{"CheckedFrame"} object.
## #'
## #' \describe{
## #'   \item{[<-}{\code{signature(x = "CheckedFrame")}: }
## #'   \item{[[<-}{\code{signature(x = "CheckedFrame")}: }
## #'   \item{[}{\code{signature(object = "CheckedFrame")}:
## #'   Returns \linkS4class{CheckedFrame} if the returned object is valid,
## #'   otherwise returns a \code{data.frame}.
## #'   }
## #'   \item{$<-}{\code{signature(x = "CheckedFrame")}: }
## #'   \item{cbind2}{\code{signature(x = "CheckedFrame")}:}
## #'   \item{rbind2}{\code{signature(x = "CheckedFrame")}: ... }
## #'   \item{names<-}{\code{signature(x = "CheckedFrame")}: ... }
## #'   \item{colnames<-}{\code{signature(object = "CheckedFrame")}: }
## #'   \item{rownames<-}{\code{signature(object = "CheckedFrame")}: }
## #'   \item{dimnames<-}{\code{signature(object = "CheckedFrame")}: }
## #' }
## #'
## #' @section Extends:
## #'
## #' \describe{
## #' \item{\code{data.frame}}{Directly.}
## #' }
## #' 
## #' @docType class
## #' @keywords classes
## #' @aliases CheckedFrame-class
## #' @aliases CheckedFrame
## #' @aliases [,CheckedFrame,missing,missing-method
## #' @aliases [,CheckedFrame,missing,ANY-method
## #' @aliases [,CheckedFrame,ANY,missing-method
## #' @aliases [,CheckedFrame,ANY,ANY-method
## #' @aliases [<-,CheckedFrame,ANY,ANY-method
## #' @aliases [<-,CheckedFrame,ANY,missing-method
## #' @aliases [<-,CheckedFrame,missing,ANY-method
## #' @aliases [<-,CheckedFrame,missing,missing-method
## #' @aliases [[<-,CheckedFrame,ANY,missing-method
## #' @aliases [[<-,CheckedFrame,ANY,ANY-method
## #' @aliases $<-,CheckedFrame-method
## #' @aliases show,CheckedFrame-method
## #' @aliases rbind2,CheckedFrame,ANY-method
## #' @aliases cbind2,CheckedFrame,ANY-method
## #' @aliases colnames<-,CheckedFrame-method
## #' @aliases rownames<-,CheckedFrame,ANY-method
## #' @aliases rownames<-,CheckedFrame,NULL-method
## #' @aliases names<-,CheckedFrame,ANY-method
## #' @aliases dimnames<-,CheckedFrame,list-method
## #' @aliases initialize,CheckedFrame-method
## #' @examples
## #' foo <- 
## #'   CheckedFrame(data.frame(a = runif(3), b = runif(3), c = letters[1:3]),
## #'                   columns = c(a = "numeric", b = "ANY", c = "factor"),
## #'                   constraints = list(function(x) {x$a > 0}))
## #' # works just like a normal data.frame
## #' print(foo)
## #' summary(foo)
## #' # errors
## #' try(foo$a <- as.character(foo$a))
## #' try(foo["a", 1] <- -1)
## #' try(foo$a <- NULL)
## #' # errors
## #' try(foo$b <- as.character(foo$b))
## #' try(foo$d <- runif(3))
## CheckedFrame <-
##   setClass("CheckedFrame", contains="data.frame",
##            representation(columns="ColumnList"
##                           exclusive="logical",
##                           ordered="logical",
##                           excluse="character",
##                           constraints="FunctionList"),
##            prototype(data.frame(),
##                      columns=ColumnList()
##                      exclusive=FALSE,
##                      exclude=character(),
##                      constraints=FunctionList()))

## setValidity("CheckedFrame",
##             function(object) {
##               rc <- validate_data_frame(object,
##                                         columns = object@columns,
##                                         exclusive = object@exclusive,
##                                         ordered = object@ordered,
##                                         exclude = object@exclude,
##                                         constraints = object@constraints)
##               if (is.character(rc)) {
##                 return(rc)
##               }
##               TRUE
##             })

## setMethod("initialize", "CheckedFrame",
##           function(.Object, x=new_data_frame(columns), columns=ColumnList(),
##                    exclusive=FALSE, ordered=FALSE, exclude=character(),
##                    constraints=list()) {
##               ## Drop any bad columns if exclusive
##             if (exclusive) {
##               coltouse <- intersect(names(x), names(columns))
##               x <- as.data.frame(x)[ , coltouse, drop=FALSE]
##             }
##             .Object <- callNextMethod(.Object, x)
##             .Object@columns <- columns
##             .Object@exclusive <- exclusive
##             .Object@exclude <- exclude
##             .Object@constraints <- FunctionList(constraints)
##             .Object
##           })

## setMethod("show", "CheckedFrame",
##           function(object) {
##             cat(sprintf("An object of class %s\n", dQuote(class(x))))
##             print(as(object, "data.frame"))
##           })

## ###Methods

## # [-method
## setMethod("[", c(x="CheckedFrame", i="missing", j="missing"),
##           function(x, i, j, drop=TRUE) {
##             if (drop && ncol(x) == 1) {
##               x[[1]]
##             } else {
##               x
##             }
##           })

## setMethod("[", c(x="CheckedFrame", i = "missing", j = "ANY"), 
##           function(x, i, j, drop=TRUE) {
##             y <- data.frame(x)[ , j, drop=drop]
##             tryCatch(new("CheckedFrame", y, x@columns, x@exclusive, x@constraints),
##                      error = function(e) y)
##           })

## setMethod("[", c(x="CheckedFrame", i = "ANY", j = "missing"), 
##           function(x, i, j, drop = TRUE) {
##             y <- as(x, "data.frame")[i, , drop=drop]
##             tryCatch(new("CheckedFrame", y, x@columns, x@exclusive, x@constraints),
##                      error = function(e) y)
##           })

## setMethod("[", c(x="CheckedFrame", i = "ANY", j = "ANY"), 
##           function(x, i, j, drop = TRUE) {
##             y <- as(x, "data.frame")[i, j, drop=drop]
##             tryCatch(new("CheckedFrame", y, x@columns, x@exclusive, x@constraints),
##                      error = function(e) y)
##           })


## # [<- method

## setMethod("[<-", c(x="CheckedFrame", i="missing", j="missing"),
##           function(x, i, j, value) {
##             y <- callGeneric(data.frame(x), , ,value=value)
##             new("CheckedFrame", y,  x@columns, x@exclusive, x@constraints)
##           })

## setMethod("[<-", c(x="CheckedFrame", i="missing", j="ANY"),
##           function(x, i, j, value) {
##             y <- callGeneric(data.frame(x), , j,value=value)
##             new("CheckedFrame", y,  x@columns, x@exclusive, x@constraints)
##           })

## setMethod("[<-", c(x="CheckedFrame", i="ANY", j="missing"),
##           function(x, i, j, value) {
##             y <- callGeneric(data.frame(x), i, ,value=value)
##             new("CheckedFrame", y,  x@columns, x@exclusive, x@constraints)
##           })

## setMethod("[<-", c(x="CheckedFrame", i="ANY", j="ANY"),
##           function(x, i, j, value) {
##             y <- callGeneric(data.frame(x), i, j,value=value)
##             new("CheckedFrame", y,  x@columns, x@exclusive, x@constraints)
##           })


## # [[<- method

## setMethod("[[<-", c(x="CheckedFrame", i="ANY", j="missing", value="ANY"),
##           function(x, i, j, value) {
##             y <- callGeneric(data.frame(x), i, value=value)
##             new("CheckedFrame", y, x@columns, x@exclusive, x@constraints)
##           })

## setMethod("[[<-", c(x="CheckedFrame", i="ANY", j="ANY", value="ANY"),
##           function(x, i, j, value) {
##             y <- callGeneric(data.frame(x), i, j, value=value)
##             new("CheckedFrame", y, x@columns, x@exclusive, x@constraints)
##           })

## # $<- method
## setMethod("$<-", "CheckedFrame",
##           function(x, name, value) {
##             y <- callNextMethod()
##             new("CheckedFrame", y, x@columns, x@exclusive, x@constraints)
##           })

## # rbind2 method
## setMethod("rbind2", "CheckedFrame",
##           function(x, y, ...) {
##             z <- rbind(as(x, "data.frame"), as(y, "data.frame"), ...)
##             new("CheckedFrame", z, x@columns, x@exclusive, x@constraints)
##           })

## # cbind2 method
## setMethod("cbind2", "CheckedFrame",
##           function(x, y, ...) {
##             z <- cbind(as(x, "data.frame"), as(y, "data.frame"), ...)
##             new("CheckedFrame", z, x@columns, x@exclusive, x@constraints)
##           })

## # colnames<-
## setMethod("colnames<-", "CheckedFrame",
##           function(x, value) {
##             y <- callNextMethod()
##             validObject(y)
##             y
##           })

## # rownames<-
## setMethod("rownames<-", c(x = "CheckedFrame", value = "ANY"),
##           function(x, value) {
##             y <- callNextMethod()
##             validObject(y)
##             y
##           })

## setMethod("rownames<-", c(x = "CheckedFrame", value = "NULL"),
##           function(x, value) {
##             x@row.names <- seq_len(nrow(x))
##             validObject(x)
##             x
##           })

## # names<-
## setMethod("names<-", "CheckedFrame",
##           function(x, value) {
##             y <- callNextMethod()
##             validObject(y)
##             y
##           })


## # names<-
## setMethod("dimnames<-", c(x="CheckedFrame", value="list"),
##           function(x, value) {
##             rownames(x) <- value[[1]]
##             colnames(x) <- value[[2]]
##             validObject(x)
##             x
##           })

