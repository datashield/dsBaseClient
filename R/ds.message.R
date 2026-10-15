#' @title Returns server-side messages to the client-side
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Server-side errors are now returned directly and can be viewed with `DSI::datashield.errors()`.
#' @param ... Not used.
#' @author DataSHIELD Development Team
#' @md
#' @keywords internal
#' @export
ds.message <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.message()", details = "Server-side errors are now returned directly and can be viewed with `DSI::datashield.errors()`.")
}
