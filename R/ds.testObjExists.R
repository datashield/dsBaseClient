#' @title Checks if an R object exists on the server-side
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Server-side functions now check that their input objects exist.
#' @param ... Not used.
#' @author DataSHIELD Development Team
#' @md
#' @keywords internal
#' @export
ds.testObjExists <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.testObjExists()", details = "Server-side functions now check that their input objects exist.")
}
