#' @include package.R
#' @include class-TableChecks.R
#' @exportMethod check_constraints
NULL

#' @docType methods
#' @title Methods for function \code{check_constraints}
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

setMethod("check_constraints", c("data.frame", "TableChecks"),
          check_constraints.data.frame.TableChecks)
