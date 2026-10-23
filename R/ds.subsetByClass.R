#' @title Generates valid subset(s) of a data frame or a factor
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Please use `ds.dataFrameSubset()` instead.
#' @param ... Not used.
#' @author Gaye, A.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @md
#' @keywords internal
#' @export
ds.subsetByClass <- function(...) {
  lifecycle::deprecate_stop("7.0.0", "ds.subsetByClass()", "ds.dataFrameSubset()")
}
