#' @include package.R
#' @include class-Columns.R
#' @exportMethod check_constraints
NULL

#' @docType methods
#' @title Methods for function \code{check_constraints}
setGeneric("check_constraints",
           function(object, checks, ...) {
             standardGeneric("check_constraints")
           })

check_constraints.ANY.ColumnChecks <- function(object, checks, name="object") {
  # check classtype
  if (checks@classtype != "ANY") {
    if (! is(object, checks@classtype)) {
      return(sprintf("%s is not a %s object",
                     dQuote(name), dQuote(checks@classtype)))
    }
  }
  # check missings
  if (!checks@missings) {
    if (any(is.na(object))) {
      return(sprintf("%s has missing values",
                     dQuote(name)))
    }
  }
  # check uniqueness
  if (checks@unique) {
    if (any(duplicated(object))) {
      return(sprintf("%s is not unique",
                     dQuote(name)))
    }
  }
  # check constraints
  for (i in seq_along(checks@constraints)) {
    f <- checks@constraints[[i]]
    if (!all(f(object))) {
      return(sprintf("%s failed constraint %s:\n%s",
                     dQuote(checks_name), names(checks@constraints)[i], deparse(f)))
    }
  }
  TRUE
}

setMethod("check_constraints", c("ANY", "ColumnChecks"),
          check_constraints.ANY.ColumnChecks)

## setMethod("check_constraints", c("data.frame", "Table"),
##           function(object, validator, ...) {
##           })

## setMethod("check_constraints", c("data.frame", "ColumnList"),
##           function(object, validator, ...) {
##           })

## check_constraints.data.frame.ColumnCheckList <- function(x, checks) {
## }

check_constraints.data.frame.TableChecks <- function(x, checks) {
  # error if any extra columns
  if (checks@exclusive) {
     badcols <- setdiff(names(x), names(checks@columns))
    if (length(badcols)) {
      return("Extra columns: %s", paste(dQuote(names(badcols)), collapse=", "))
    }
  }
  # Check that columns are in order
  if (checks@ordered) {
    n <- length(checks@columns)
    inorder <- (names(x)[seq_len(n)] == names(checks@columns))
    if (!all(inorder)) {
      return(sprintf("Checks@Columns not in order\nExpected order:%s\nOut of order columns:%s",
                     paste(dQuote(names(checks@columns)), collapse=","),
                     paste(dQuote(names(x)[!inorder]), collapse=",")))
    }
  }
  # Check for excluded columns
  if (length(checks@exclude)) {
    badcols <- intersect(names(x), checks@exclude)
    if (length(badcols)) {
      return(sprintf("Columns which should not be in the data.frame:\n%s",
                     paste(dQuote(bacols), collapse=",")))
    }
  }

  # check all columns 
  for (i in seq_along(checks@columns)) {
    column_name <- names(checks@columns)[i]
    column <- checks@columns[[i]]
    # check existence
    if (! column_name %in% names(x)) {
      return(sprintf("Column %s not present", dQuote(column_name)))
    } else {
    }
    # check global constraints
    for (i in seq_along(checks@constraints)) {
      f <- checks@constraints[[i]]
      if (!f(x)) {
        return(sprintf("Failed constraint %s\n:%s",
                       names(checks@constraints)[i], deparse(f)))
      }
    }
  }
  TRUE

}
