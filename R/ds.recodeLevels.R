#' @title Recodes the levels of a server-side factor vector
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.recodeValues()` instead.
#' @param ... Not used.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @md
#' @keywords internal
#' @export
ds.recodeLevels <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.recodeLevels()", "ds.recodeValues()")
}
