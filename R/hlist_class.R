#' @include package.R
#' @include class-HList.R
#' @export hlist_class
#' @exportClass FunctionList
#' @export FunctionList
NULL

#' Create a subclass of \code{HList}
#'
#' Create a subclass of \code{\linkS4class{HList}}, which restricts elements
#' of the list to be a specified class.
#'
#' @param Class \code{character} string name of the new class
#' that will extend \code{HList}.
#' @param classtype \code{character} The name of the class which 
#' all elements must inherit from.
#' @param where Passed to \code{\link{setClass}}. The environment
#' in which to store the definition.
#'
#' @return \code{function} with usage \code{function(...)}, which
#' will create objects of the newly defined class.
#'
#' @seealso \code{"\linkS4class{FunctionList}"} for a subclass 
#' created with this function used in this section. \code{\linkS4class{HList}}.
#' @examples
#' NumericList <- 
#'   hlist_class("NumericList", "numeric")
#' # creates a new class "NumericList"
#' showClass("NumericList")
#' # Create a new object of class NumericList
#' foo <- NumericList(a=1, b=2)
#' print(foo)
#' foo[["c"]] <- 3
#' print(foo)
#' # error
#' try(foo[["c"]] <- 3)
hlist_class <- function(Class, classtype="ANY",
                        where=topenv(parent.frame())) {

  ## if (isClass(Class, where=where)) {
  ##   print("Class  exists")
  ##   removeClass(Class, where=where)
  ##   print(isClass(Class, where=where))
  ## } else {
  ##   print("Class does not exist")
  ## }
  
  setClass(Class,
           contains="HList",
           prototype=prototype(list(), classtype=classtype),
           where=where)
  
  setMethod("initialize", Class,
            function(.Object, x=list()) {
              callNextMethod(.Object, x, classtype=classtype)
            }, where=where)

  setValidity(Class,
              function(object) {
                if (object@classtype != classtype) {
                  return(sprintf("object@classtype != %s", classtype))
                }
                    TRUE
              },
              where=where)
  
  setMethod("show", Class,
            function(object) {
              cat(sprintf("An object of class %s\n", dQuote(Class)))
              callGeneric(as(object, "HList"))
            }, where=where)
  
  setMethod("[", signature = c(x = Class, i = "missing", j="ANY"),
            function(x, i, j, drop) {
              x
            }, where=where)
  
  setMethod("[", signature = c(x = Class, i = "ANY", j="ANY"), 
            function(x, i, j, drop) {
              y <- callGeneric(as(x, "HList"), i=i)
              new(Class, y)
            }, where = where)
  
  setMethod("[<-", signature = c(x = Class, i = "missing"), 
            function(x, i, j, ..., value) {
              y <- callGeneric(as(x, "HList"), value=value)
              new(Class, y)
            }, where = where)
  
  setMethod("[<-", signature = c(x = Class, i = "ANY"), 
            function(x, i, j, ..., value) {
              y <- callGeneric(as(x, "HList"), i, value=value)
              new(Class, y)
            }, where = where)
  
  setMethod("[[<-", signature=c(x=Class, i = "missing", value = "ANY"),
            function(x, i, j, ..., value) {
              # Error ... : [[ ]] with missing subscript
              callGeneric(as(x, "HList"), value=value)
            }, where = where)
  
  setMethod("[[<-", signature = c(x = Class, i = "ANY", value = "ANY"),
            function(x, i, j, ..., value) {
              y <- callGeneric(as(x, "HList"), i=i, value=value)
              new(Class, y)
            }, where = where)
  
  setMethod("$<-", signature = c(x = Class),
            function(x, name, value) {
              x[[name]] <- value
              x
            }, where = where)
  
  setMethod("c", signature = Class,
            function(x, ...) {
              y <- callGeneric(as(x, "HList"), ...)
              new(Class, y)
            }, where = where)
  
  setMethod("names<-", signature=c(x = Class, value="NULL"),
            function(x, value) {
              x@names <- rep(NA_character_, length(x))
              x
            }, where = where)
  
  setMethod("length<-", signature=c(x = Class, value="numeric"),
            function(x, value) {
              y <- callGeneric(as(x, "HList"), value)
              new(Class, y)
            }, where = where)
  
  setAs("list", Class,
        function(from, to) new(class, from), where=where)

  .f <- function(...) {
    new(Class, nlist(...))
  }
  
  invisible(.f)
}

#' @docType class
#' @aliases FunctionList
#' @aliases FunctionList-class
#' 
#' @title List of Functions
#'
#' @description A class which is a list of functions. This
#' is a subclass of \code{"\linkS4class{HList}"}, and was
#' itself created using the function \code{\link{hlist_class}}.
#'
#' @param ... Objects of class \code{function}.
#'
#' @section Extends:
#'
#' \describe{
#' \item{\code{HList}}{directly}
#' }
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{.Data}:}{\code{"list"}}
#'    \item{\code{classtype}:}{\code{"character"} Always equal to \code{"function"} in this class.}
#'    \item{\code{names}:}{\code{"character"}}
#'  }
#'
#' @seealso \code{\link{hlist_class}}, \code{\linkS4class{HList}}, 
#' @examples
# The code used to create it
# FunctionList <- hlist_class("FunctionList", "function")
#' flist <- FunctionList(sum = sum, mean=mean)
#' flist2 <- c(flist, list(max=max))
#' x <- rnorm(35)
#' lapply(flist2, function(f) f(x))
#' # cannot add non-functions to it
#' try(flist[["foo"]] <- 1)
FunctionList <- hlist_class("FunctionList", "function")
