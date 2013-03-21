#' @include package.R
#' @include subclass_homog_list.R
NULL

Column <-
  setClass("Column",
           representation(type = "character",
                          missings = "logical",
                          unique = "logical",
                          constraints = "FunctionList"),
           prototype(type = "ANY",
                     missings = TRUE,
                     unique = FALSE,
                     constraints = FunctionList()))

ColumnList <- subclass_homog_list("ColumnList", "Column")

Validator <-
  setClass("Validator",
           representation(columns = "ColumnList",
                          exclusive = "logical",
                          ordered = "logical",
                          exclude = "character",
                          constraints = "FunctionList"))


#' Validate Data Frame
#'
#' @param x \code{data.frame}. Data frame to be checked.
#' @param columns \code{ColumnList} containing column level checks.
validate_data_frame <- function(x, validator) {
  # error if any extra columns
  if (validator@exclusive) {
     badcols <- setdiff(names(x), names(validator@columns))
    if (length(badcols)) {
      return("Extra columns: %s", paste(dQuote(names(badcols)), collapse=", "))
    }
  }
  # Check that columns are in order
  if (validator@ordered) {
    n <- length(validator@columns)
    inorder <- (names(x)[seq_len(n)] == names(validator@columns))
    if (!all(inorder)) {
      return(sprintf("Validator@Columns not in order\nExpected order:%s\nOut of order columns:%s",
                     paste(dQuote(names(validator@columns)), collapse=","),
                     paste(dQuote(names(x)[!inorder]), collapse=",")))
    }
  }
  # Check for excluded columns
  if (length(validator@exclude)) {
    badcols <- intersect(names(x), validator@exclude)
    if (length(badcols)) {
      return(sprintf("Columns which should not be in the data.frame:\n%s",
                     paste(dQuote(bacols), collapse=",")))
    }
  }

  # check all columns 
  for (i in seq_along(validator@columns)) {
    column_name <- names(validator@columns)[i]
    column <- validator@columns[[i]]
    # check existence
    if (! column_name %in% names(x)) {
      return(sprintf("Column %s not present", dQuote(column_name)))
    } else {
      # check classtype
      if (column@classtype != "ANY") {
        if (! is(x[[column_name]], column@classtype)) {
          return(sprintf("Column %s is not a %s object",
                         dQuote(column_name), dQuote(column@classtype)))
        }
      }
      # check missings
      if (!column@missings) {
        if (any(is.na(x[[column_name]]))) {
          return(sprintf("Column %s has missing values",
                         dQuote(column_name)))
        }
      }
      # check uniqueness
      if (column@unique) {
        if (any(duplicated(x[[column_name]]))) {
          return(sprintf("Column %s is not unique",
                         dQuote(column_name)))
        }
      }
      # check constraints
      for (i in seq_along(column@constraints)) {
        f <- column@constraints[[i]]
        if (!all(f(x[[column_name]]))) {
          return(sprintf("Column %s failed constraint %s:\n%s",
                         dQuote(column_name), names(column@constraints)[i], deparse(f)))
        }
      }
    }
    # check global constraints
    for (i in seq_along(validator@constraints)) {
      f <- validator@constraints[[i]]
      if (!f(x)) {
        return(sprintf("Failed constraint %s\n:%s",
                       names(validator@constraints)[i], deparse(f)))
      }
    }
  }
  TRUE
}
