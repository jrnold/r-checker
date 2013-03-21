#' @include package.R
#' @export nlist
NULL

#' Named List
#'
#' Create a \code{"namedList"}, the S4 class
#' equivalent to primitive lists.
#'
#' @param ... objects, possibly named.
#' @return An object of class \code{"namedList"}.
#' @examples
#' nlist(a=1, b="a", c=3L)
nlist <- function(...) {
  as(list(...), "namedList")
}
