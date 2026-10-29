#' @title Performs a mathematical operation on two or more vectors
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.make()` instead.
#' @param ... Not used.
#' @author Gaye, A.
#' @md
#' @keywords internal
#' @export
ds.vectorCalc <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.vectorCalc()", "ds.make()")
}
