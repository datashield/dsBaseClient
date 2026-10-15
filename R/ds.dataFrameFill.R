#' @title Creates missing values columns in the server-side
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.standardiseDf()` instead.
#' @param ... Not used.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @md
#' @keywords internal
#' @export
ds.dataFrameFill <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.dataFrameFill()", "ds.standardiseDf()")
}
