#' @include package.R
#' @exportClass HList
#' @export HList
#' @exportMethod [
#' @exportMethod [<-
#' @exportMethod [[<-
#' @exportMethod $<-
#' @exportMethod c
#' @exportMethod names<-
#' @exportMethod length<-
NULL

is_or_null <- function(object, class2) {
  is(object, class2) || is.null(object)
}

#' @rdname HList-class
#' @aliases HList-class
#' @aliases HList
#' @aliases [,HList,missing,ANY-method
#' @aliases [,HList,ANY,ANY-method
#' @aliases [<-,HList,missing,ANY-method
#' @aliases [<-,HList,ANY,ANY-method
#' @aliases [[<-,HList,missing,ANY-method
#' @aliases [[<-,HList,ANY,ANY-method
#' @aliases $<-,HList-method
#' @aliases c,HList-method
#' @aliases length<-,HList,numeric-method
#' @aliases names<-,HList,NULL-method
#' @aliases show,HList-method
#' @docType class
#' @keywords classes
#' 
#' @title Homogenous lists
#'
#' @description An S4 subclass of \code{list} in which all elements of the
#' list to be the same class. This is similar to the 'atomic lists' in R in that all elements
#' of the vector must be the same class, but the \code{HList}
#' supports arbitrary classes. \code{NULL} values are also valid.
#' 
#' @section Slots:
#'
#' \describe{
#' \item{\code{.Data}}{Object of class \code{list}.}
#' \item{\code{classtype}}{\code{character}. Required classtype for
#' all elements in the list.}
#' \item{\code{empty_names}}{\code{logical}. If \code{FALSE}, then no names can
#' be \code{NA} or \code{""}.}
#' \item{\code{unique_names}}{\code{logical}. If \code{TRUE}, then no duplicate
#' names are allowed.}
#' }
#'
#' @param x \code{list}.
#' @param classtype \code{character}
#' @param ... Data to include in the new object. Named arguments correspond to
#' slots in the class definition.
#' 
#' @section Extends:
#'
#' \describe{
#' \item{\code{list}}{Directly.}
#' }
#'
#' @section Methods:
#' 
#' \describe{
#'     \item{[<-}{\code{signature(x = "HList")}}
#'     \item{[}{\code{signature(x = "HList")}}
#'     \item{[[<-}{\code{signature(x = "HList")}}
#'     \item{c}{\code{signature(x = "HList")}}
#'     \item{show}{\code{signature(object = "HList")}}
#' }
#' @seealso \code{\link{hlist_class}}, 
#' @examples
#' # This is valid with lists
#' foo <- list(a=1:10, b=c(1, 5))
#' foo[["c"]] <- c("a", "z")
#' # # But would not be valid with a homogenous list
#' foo <- HList(list(a=1:10, b=c(1, 5)), "numeric")
#' try(foo[["c"]] <- c("a", "z"))
#' foo[["c"]] <- c(1, 4, 10)
setClass("HList",
         contains="namedList",
         representation(classtype="character",
                        unique_names = "logical",
                        empty_names = "logical"),
         prototype(list(),
                   classtype="ANY",
                   unique_names = FALSE,
                   empty_names = TRUE))


#' @rdname HList-class
HList <- function(x, classtype="ANY", ...) {
  new("HList", x, classtype=classtype, ...)
}

is_empty_string <- function(x) {
  is.na(x) | x == ""
}

setValidity("HList",
            function(object) {
                if (length(object@classtype) != 1) {
                    return("object@classtype has a length != 1")
                }
                # Hack. need to test s3 and s4 classes differently
                # is(x, "ANY") does not work for s3 objects
                if (object@classtype != "ANY") {
                  if (!all(sapply(object, is_or_null, class2=object@classtype))) {
                    return(sprintf("Not all elements have class %s",
                                   object@classtype))
                  }
                }
                if (!object@empty_names) {
                  if (any(sapply(names(object), is_empty_string))) {
                    return("Empty names are not allowed")
                  }
                }
                if (object@unique_names) {
                  if (any(duplicated(names(object)))) {
                    return("Duplicate names are not allowed")
                  }
                }
                TRUE
              })

setMethod("show", "HList",
          function(object) {
            cat(sprintf("List of %s objects\n", dQuote(object@classtype)))
            print(structure(object@.Data, names = object@names))
          })

### Methods
setMethod("c", signature="HList",
          def=function(x, ...) {
            y <- callGeneric(as(x, "namedList"), ...)
            new("HList", y, classtype=x@classtype,
                empty_names = x@empty_names, unique_names = x@unique_names)
          })

setMethod("[", signature=c(x="HList", i="missing"), 
          function(x, i, j, ...., drop) x)

setMethod("[", signature=c(x="HList", i="ANY"), 
          function(x, i, j, ...., drop) {
            y <- callGeneric(as(x, "namedList"), i=i)
            new("HList", y, classtype=x@classtype,
                empty_names = x@empty_names, unique_names = x@unique_names)
          })

setMethod("[<-", signature=c(x="HList", i="missing"), 
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), value=value)
            new("HList", y, classtype=x@classtype,
                empty_names = x@empty_names, unique_names = x@unique_names)
          })

setMethod("[<-", signature=c(x="HList", i="ANY"), 
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i, value=value)
            new("HList", y, classtype=x@classtype,
                empty_names = x@empty_names, unique_names = x@unique_names)
          })

setMethod("[[<-", signature=c(x="HList", i="missing", value="ANY"),
          function(x, i, j, ..., value) {
            stop("[[ ]] with missing subscript")
          })

setMethod("[[<-", signature=c(x="HList", i="ANY", value="ANY"),
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i=i, value=value)
            new("HList", y, classtype=x@classtype,
                empty_names = x@empty_names, unique_names = x@unique_names)
          })

setMethod("$<-", signature=c(x="HList"),
          function(x, name, value) {
            x[[name]] <- value
            x
          })

setMethod("names<-", signature=c(x="HList", value="NULL"),
          function(x, value) {
            x@names <- rep(NA_character_, length(x))
            validObject(x)
            x
          })

setMethod("length<-", signature=c(x="HList", value="numeric"),
          function(x, value) {
            y <- callGeneric(as(x, "namedList"), value)
            new("HList", y,
                classtype=x@classtype,
                empty_names = x@empty_names, unique_names = x@unique_names)
          })

