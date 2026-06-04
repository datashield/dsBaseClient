#' Retrieve datasources if not specified
#'
#' @param datasources An optional list of data sources. If not provided, the function will attempt
#' to find available data sources.
#' @importFrom DSI datashield.connections_find
#' @return A list of data sources.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.get_datasources <- function(datasources) {
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  return(datasources)
}

#' Verify that the provided data sources are of class 'DSConnection'.
#'
#' @param datasources A list of data sources.
#' @importFrom cli cli_abort
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.verify_datasources <- function(datasources) {
  is_connection_class <- sapply(datasources, function(x) inherits(unlist(x), "DSConnection"))
  if (!all(is_connection_class)) {
    cli_abort("The 'datasources' were expected to be a list of DSConnection-class objects")
  }
}

#' Set and verify data sources.
#'
#' @param datasources An optional list of data sources. If not provided, the function will attempt
#' to find available data sources.
#' @return A list of verified data sources.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.set_datasources <- function(datasources) {
  datasources <- .get_datasources(datasources)
  .verify_datasources(datasources)
  return(datasources)
}

#' Check cross-study class consistency from a list of server aggregate results
#'
#' Batch-refactored server functions return a list per study that includes a
#' `class` field. This helper verifies that the class field is identical across
#' all studies and aborts if not.
#'
#' @param results A named list of server-side aggregate results, one per study,
#'   each containing a `class` element.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkClassConsistency <- function(results) {
  classes <- lapply(results, function(r) r$class)
  if (length(unique(lapply(classes, sort))) > 1) {
    cli_abort("The input object is not of the same class in all studies!")
  }
}

#' Check That a Data Frame Name Is Provided
#'
#' Internal helper that checks whether a data frame or matrix object
#' has been provided. If `NULL`, it aborts with a user-friendly error.
#'
#' @param df A data.frame or matrix.
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.check_df_name_provided <- function(df) {
  if(is.null(df)){
    cli_abort("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }
}
