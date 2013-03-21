#' @include package.R
#' @include hlist_class.R
#' @export ColumnChecks
#' @export ColumnCheckList
#' @export TableChecks
NULL

empty_character <- function(x) {
  (is.na(x) | x == "")
}

#' @docType class
#' @aliases ColumnChecks-class
#' @aliases ColumnChecks
#' @title Class \code{ColumnChecks}
#'
#' @description An object containing column-level checks. The following
#' constraint checks are implemented:
#'
#' \itemize{
#' \item class type
#' \item uniqueness
#' \item missing values
#' \item list of arbitrary constraints
#' }
#'
#' @param ... Data to be included in the object.
#' 
#' @section Objects from the Class:
#' 
#' Objects can be created by calls of the form \code{ColumnChecks(...)}.
#'
#' @section Slots:
#' \describe{
#' \item{\code{type}}{\code{character}. Class type of object to be checked.}
#' \item{\code{missings}}{\code{logical}. If \code{FALSE}, then no \code{NA} values are allowed in the object to be checked.}
#' \item{\code{uniqueness}}{\code{logical}. If \code{FALSE}, then duplicate values are allowed in the object to be checked.}
#'     \item{\code{constraints}:}{\code{"FunctionList"}. Additional arbitrary constraints. Each function must return either \code{TRUE} if the constraint is satisfied, or \code{FALSE} if it is violated. These functions can also return vectors, in which case, the constraint is violated if there is any \code{FALSE} value.}
#' }
#'
#' @family Check objects
#' @examples
#' showClass("ColumnChecks")
ColumnChecks <-
  setClass("ColumnChecks",
           representation(classtype = "character",
                          missings = "logical",
                          unique = "logical",
                          constraints = "FunctionList"),
           prototype(classtype = "ANY",
                     missings = TRUE,
                     unique = FALSE,
                     constraints = FunctionList()))

#' @rdname ColumnCheckList-class
#' @docType class
#' @aliases ColumnCheckList-class
#' @aliases ColumnCheckList
#' @title Class \code{ColumnCheckList}
#'
#' @description An object containing multiple column-level constraint checks.
#'
#' @param ... Objects of class \code{\linkS4class{ColumnChecks}} to include.
#' All arguments must be named. The names of the \code{ColumnChecks} objects
#' will correspond with their names in the \code{data.frame} to be checked.
#' 
#' @section Objects from the Class:
#' 
#' Objects can be created by calls of the form \code{ColumnCheckList(...)}.
#'
#' @section Extends:
#'   \describe{
#'     \item{\code{\linkS4class{HList}}}{directly.}
#'   }
#' @family Check objects
#' @examples
#' showClass("ColumnCheckList")
setClass("ColumnCheckList",
         contains="namedList",
         representation=representation(classtype = "character"),
         prototype=prototype(nlist(), classtype="ColumnChecks"))

validity.ColumnCheckList <- function(object) {
  if (!all(sapply(object, is, class2=object@classtype))) {
    return(sprintf("Not all elements inherit from class %s",
                   dQuote(object@classtype)))
  }
  if (any(sapply(names(object), empty_character))) {
    return(sprintf("Names cannot be empty"))
  }
  TRUE
}

setValidity("ColumnCheckList", validity.ColumnCheckList)

#' @rdname ColumnCheckList-class
ColumnCheckList <- function(...) {
  new("ColumnCheckList", nlist(...))
}

#' @docType class
#' @aliases TableChecks-class
#' @aliases TableChecks
#' @title Class \code{ColumnChecks}
#'
#' @description An object containing constraint checks intended
#' to be used on a \code{data.frame}.
#'
#' @param ... Data to include in the new object. Named arguments
#' correspond to slots in the class definition.
#' 
#' @section Objects from the Class:
#' 
#' Objects can be created by calls of the form \code{TableChecks(...)}.
#'
#' @section Slots:
#'   \describe{
#'     \item{\code{columns}:}{\code{"ColumnCheckList"}. Column-level constraints.}
#'     \item{\code{exclusive}:}{\code{"logical"}. If \code{TRUE}, then the \code{data.frame} can only contain the columns in \code{columns}. If \code{FALSE}, the \code{data.frame} still must contain the columns in \code{columns}, but can contain additional columns.}
#'     \item{\code{ordered}:}{\code{"logical"}. If \code{TRUE}, the columns in the \code{data.frame} must appear in the same order as they are listed in \code{columns}, and those columns must be the first columns in the \code{data.frame}. If \code{FALSE}, the the columns in \code{columns} can appear anywhere in the \code{data.frame}.}
#'     \item{\code{exclude}:}{\code{"character"}. A list of column names which cannot be in the data frame.}
#'     \item{\code{constraints}:}{\code{"FunctionList"}. Additional arbitrary constraints. Each function must return either \code{TRUE} if the constraint is satisfied, or \code{FALSE} if it is violated.}
#'   }
#' @family Check objects
#' @examples
#' showClass("TableChecks")
TableChecks <-
  setClass("TableChecks",
           representation(columns = "ColumnCheckList",
                          exclusive = "logical",
                          ordered = "logical",
                          exclude = "character",
                          constraints = "FunctionList"),
           prototype(columns = ColumnCheckList(),
                     exclusive = FALSE,
                     ordered = FALSE,
                     exclude = character(),
                     constraints = FunctionList()))

