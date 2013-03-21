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

check_constraints.ANY.ColumnChecks <- function(object, checks, name="object") {
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
      if (constr_name == "") {
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

check_constraints.data.frame.TableChecks <- function(x, checks) {
  # check all columns 
  for (i in seq_along(checks@columns)) {
    column_name <- names(checks@columns)[i]
    column <- checks@columns[[i]]
    # check existence
    if (! column_name %in% names(x)) {
      return(sprintf("Column %s not present", dQuote(column_name)))
    } else {
      rc <- check_constraints(checks@columns)
      if (is.character(rc)) {
        return(rc)
      }
    }
  }
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
  # check global constraints
  for (i in seq_along(checks@constraints)) {
    f <- checks@constraints[[i]]
    if (!all(f(x))) {
      constr_name <- names(checks@constraints)[i]
      if (constr_name == "") {
        constr_name <- sprintf("#%d", i)
      }
      return(sprintf("Failed constraint %s", constr_name))
    }
  }
  TRUE
}

setMethod("check_constraints", c("data.frame", "Table"),
          check_constraints.data.frame.TableChecks)
