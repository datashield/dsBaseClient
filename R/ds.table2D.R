#' @title Generates 2-dimensional contingency tables
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.table()` instead.
#' @param ... Not used.
#' @author Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development Team
#' @md
#' @keywords internal
#' @export
ds.table2D <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.table2D()", "ds.table()")
}
