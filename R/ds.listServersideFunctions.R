#' @title Lists server-side functions
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `DSI::datashield.methods()` instead.
#' @param ... Not used.
#' @author DataSHIELD Development Team
#' @md
#' @keywords internal
#' @import DSI
#' @export
ds.listServersideFunctions <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.listServersideFunctions()", "DSI::datashield.methods()")
}
