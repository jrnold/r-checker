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

#' Homogenous lists
#'
#' An S4 subclass of \code{list} in which all elements of the
#' list to be the same class.
#'
#' This is similar to the 'atomic lists' in R in that all elements
#' of the vector must be the same class, but the \code{HList}
#' supports arbitrary classes. \code{NULL} values are also valid.
#' 
#' @section Slots:
#'
#' \describe{
#' \item{\code{.Data}}{Object of class \code{list}.}
#' \item{\code{classtype}}{Object of class \code{character}. Required classtype for
#' all elements in the list.}
#' }
#'
#' @param ... Passed onto generic functions.
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
#'     \item{[<-}{\code{signature(x = "HList")}: ... }
#'     \item{[}{\code{signature(x = "HList")}: ... }
#'     \item{[[<-}{\code{signature(x = "HList")}: ... }
#'     \item{c}{\code{signature(x = "HList")}: ... }
#'     \item{show}{\code{signature(object = "HList")}: ... }
#' }
#' 
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
#' @exportClass HList
#' @export
#' @examples
#' foo <- HList(list(sum=sum, max=max, min=min), "function")
#' print(foo)
#' x <- 1:10
#' lapply(foo, function(f) f(x))
#' foo[["mean"]] <- mean
#' print(foo)
#' # error
#' try(foo[["a"]] <- 1)
HList <- setClass("HList",
                      contains="namedList",
                      representation(classtype="character"),
                      prototype(list(), classtype="ANY"))

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
                TRUE
              })

setMethod("initialize", "HList",
          function(.Object, x=list(), classtype="ANY") {
            .Object <- callNextMethod(.Object, x)
            .Object@classtype <- classtype
            validObject(.Object)
            .Object
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
            new("HList", y, classtype=x@classtype)
          })

setMethod("[", signature=c(x="HList", i="missing"), 
          function(x, i, j, ...., drop) x)

setMethod("[", signature=c(x="HList", i="ANY"), 
          function(x, i, j, ...., drop) {
            y <- callGeneric(as(x, "namedList"), i=i)
            new("HList", y, classtype=x@classtype)
          })

setMethod("[<-", signature=c(x="HList", i="missing"), 
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), value=value)
            new("HList", y, classtype=x@classtype)
          })

setMethod("[<-", signature=c(x="HList", i="ANY"), 
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i, value=value)
            new("HList", y, classtype=x@classtype)
          })

setMethod("[[<-", signature=c(x="HList", i="missing", value="ANY"),
          function(x, i, j, ..., value) {
            stop("[[ ]] with missing subscript")
          })

setMethod("[[<-", signature=c(x="HList", i="ANY", value="ANY"),
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i=i, value=value)
            new("HList", y, classtype=x@classtype)
          })

setMethod("$<-", signature=c(x="HList"),
          function(x, name, value) {
            x[[name]] <- value
            x
          })

setMethod("names<-", signature=c(x="HList", value="NULL"),
          function(x, value) {
            x@names <- rep(NA_character_, length(x))
            x
          })

setMethod("length<-", signature=c(x="HList", value="numeric"),
          function(x, value) {
            y <- callGeneric(as(x, "namedList"), value)
            new("HList", y, classtype=x@classtype)
          })

