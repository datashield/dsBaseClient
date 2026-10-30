#' @title Lists all Opal objects in the analytic environment
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `DSI::datashield.connections()` instead.
#' @param ... Not used.
#' @author Burton, PR. 28/9/16
#' @md
#' @keywords internal
#' @export
ds.listOpals <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.listOpals()", "DSI::datashield.connections()")
}
