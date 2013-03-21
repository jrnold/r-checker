#' @include package.R
#' @include class-HList.R
#' @export subclass_homog_list
NULL

#' Create a subclass of \code{HList}
#'
#' Creates a new subclass of \code{HList} for a specific class.
#'
#' @param Class \code{character} string name of the new class
#' that will extend \code{HList}.
#' @param classtype \code{character} The name of the class which 
#' all elements must inherit from. This is tested with \code{is}.
#' @param where Passed to \code{\link{setClass}}.
#'
#' @examples
#' NumericList <- 
#'   subclass_homog_list("NumericList", "numeric")
#' # creates a new class "NumericList"
#' showClass("NumericList")
#' # Create a new object of class NumericList
#' foo <- NumericList(list(a=1, b=2))
#' print(foo)
#' foo[["c"]] <- 3
#' print(foo)
#' # error
#' try(foo[["c"]] <- 3)
subclass_homog_list <- function(Class, classtype="ANY",
                                where=topenv(parent.frame())) {
    .f <- setClass(Class,
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
    
    invisible(.f)
}

# Used later
FunctionList <- subclass_homog_list("FunctionList", "function")
