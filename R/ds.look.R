#' @title Performs direct call to a server-side aggregate function
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Its functionality has been removed.
#' @param ... Not used.
#' @author DataSHIELD Development Team
#' @md
#' @keywords internal
#' @export
ds.look <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.look()", details = "Its functionality has been removed.")
}
