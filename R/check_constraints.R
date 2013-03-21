#' @include package.R
#' @include class-TableChecks.R
#' @exportMethod check_constraints
NULL

#' @name check_constraints-methods
#' @rdname check_constraints-methods
#' @docType methods
#' @aliases check_constraints
#' @aliases check_constraints-methods
#' @aliases check_constraints,ANY,ColumnChecks-method
#' @aliases check_constraints,ANY,ColumnChecks-method
#' @title Methods for function \code{check_constraints}
#'
#' @description Methods which check one object against
#' another object containing constraints which must be met.
#' Most commonly a \code{data.frame} is checked against
#' constraints in a \code{\linkS4class{TableChecks}} object.
#' 
#' @section Methods:
#' \describe{
#' \item{\code{signature(object = "ANY", checks = "ColumnChecks")}}{
#' }
#' \item{\code{signature(object = "data.frame", checks = "TableChecks")}}{
#' }
#' }
#'
#' 
#' @usage \code{check_constraints(object, checks, ...)}
#' @param object The object to be checked.
#' @param checks The object containing the constraints.
#' @return \code{TRUE} if all constraints are met. If a
#' constraint is failed, a \code{character} vector describing
#' the problems.
#' @examples
#' # create a TableChecks object
#' # Require a numeric column foo and a factor column bar#
#' # Also require foo to be positive
#' positive <- function(x) x > 0
#' columns <- ColumnCheckList(foo = ColumnChecks(classtype = "numeric",
#'                              constraints = FunctionList(postive=positive)),
#'                            bar = ColumnChecks(classtype = "factor"))
#' # Require that the data.frame can contain ONLY foo and bar
#' checks <- TableChecks(columns = columns, exclusive = TRUE)
#' # this data frame is valid
#' check_constraints(data.frame(foo = 1:26, bar = letters),
#'                   checks)
#' # these are invalid
#' check_constraints(data.frame(foo = -(1:26), bar=letters), checks)
#' check_constraints(data.frame(foo = 1:26, bar=1:26), checks)
#' check_constraints(data.frame(foo = 1:26, bar=letters, baz=letters),
#'                   checks)
setGeneric("check_constraints",
           function(object, checks, ...) {
             standardGeneric("check_constraints")
           })

check_constraints.ANY.ColumnChecks <- function(object, checks) {
  name <- "object"
  # check type
  if (checks@classtype != "ANY") {
    if (! is(object, checks@classtype)) {
      return(sprintf("%s does not inherit from %s",
                     sQuote(name), dQuote(checks@classtype)))
    }
  }
  # check missings
  if (!checks@missings) {
    if (any(is.na(object))) {
      return(sprintf("%s has missing values",
                     sQuote(name)))
    }
  }
  # check uniqueness
  if (checks@unique) {
    if (any(duplicated(object))) {
      return(sprintf("%s has duplicated values",
                     sQuote(name)))
    }
  }
  # check constraints
  for (i in seq_along(checks@constraints)) {
    f <- checks@constraints[[i]]
    if (!all(f(object))) {
      constr_name <- names(checks@constraints)[i]
      if (empty_character(constr_name)) {
        constr_name <- sprintf("#%d", i)
      } else {
        constr_name <- dQuote(constr_name)
      }
      return(sprintf("Failed constraint %s", constr_name))
    }
  }
  TRUE
}

#' @rdname check_constraints-methods
setMethod("check_constraints", c("ANY", "ColumnChecks"),
          check_constraints.ANY.ColumnChecks)

check_constraints.data.frame.TableChecks <- function(object, checks) {
  # check columns
  for (i in seq_along(checks@columns)) {
    column_name <- names(checks@columns)[i]
    column <- checks@columns[[i]]
    # check existence
    if (! column_name %in% names(object)) {
      return(sprintf("Column %s not present", dQuote(column_name)))
    } else {
      # check column level constraints
      rc <- check_constraints(object[[column_name]], column)
      if (is.character(rc)) {
        return(sprintf("Invalid column %s: %s",
                       dQuote(column_name), rc))
      }
    }
  }
  # error if any extra columns
  if (checks@exclusive) {
     badcols <- setdiff(names(object), names(checks@columns))
    if (length(badcols)) {
      msg <- sprintf("Extra columns: %s",
                    paste(dQuote(badcols), collapse=", "))
      return(msg)
    }
  }
  # Check that columns are in order
  if (checks@ordered) {
    n <- length(checks@columns)
    inorder <- (names(object)[seq_len(n)] == names(checks@columns))
    if (!all(inorder)) {
      return(sprintf("Columns not in order\nExpected order:%s\nOut of order columns:%s",
                     paste(dQuote(names(checks@columns)), collapse=","),
                     paste(dQuote(names(object)[!inorder]), collapse=",")))
    }
  }
  # Check for excluded columns
  if (length(checks@exclude)) {
    badcols <- intersect(names(object), checks@exclude)
    if (length(badcols)) {
      return(sprintf("Columns which should not be in the data.frame: %s",
                     paste(dQuote(badcols), collapse=",")))
    }
  }
  # check global constraints
  for (i in seq_along(checks@constraints)) {
    f <- checks@constraints[[i]]
    if (!all(f(object))) {
      constr_name <- names(checks@constraints)[i]
      if (empty_character(constr_name)) {
        constr_name <- sprintf("#%d", i)
      } else {
        constr_name <- dQuote(constr_name)
      }
      return(sprintf("Failed constraint %s", constr_name))
    }
  }
  TRUE
}

#' @rdname check_constraints-methods
setMethod("check_constraints", c("data.frame", "TableChecks"),
          check_constraints.data.frame.TableChecks)
