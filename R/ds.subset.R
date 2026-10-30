#' @title Generates a valid subset of a table or a vector
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.dataFrameSubset()` instead.
#' @param ... Not used.
#' @author Gaye, A.
#' @md
#' @keywords internal
#' @export
ds.subset <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.subset()", "ds.dataFrameSubset()")
}
