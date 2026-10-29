#' @title Creates a default set of Opal objects called 'default.opals'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `DSI::datashield.connections_default()` instead.
#' @param ... Not used.
#' @author Burton, PR. 28/9/16
#' @md
#' @keywords internal
#' @export
ds.setDefaultOpals <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.setDefaultOpals()", "DSI::datashield.connections_default()")
}
