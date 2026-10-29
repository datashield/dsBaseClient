#' @title Generates 1-dimensional contingency tables
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.table()` instead.
#' @param ... Not used.
#' @author Gaye, A.; Burton, P.
#' @md
#' @keywords internal
#' @export
ds.table1D <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.table1D()", "ds.table()")
}
