#' @title Computes the mean and standard deviation across categories
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.meanSdGp()` instead.
#' @param ... Not used.
#' @author DataSHIELD Development Team
#' @md
#' @keywords internal
#' @export
ds.meanByClass <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.meanByClass()", "ds.meanSdGp()")
}
