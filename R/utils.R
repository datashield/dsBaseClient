#' Retrieve datasources if not specified
#'
#' @param datasources An optional list of data sources. If not provided, the function will attempt
#' to find available data sources.
#' @importFrom DSI datashield.connections_find
#' @return A list of data sources.
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
#' @noRd
.set_datasources <- function(datasources) {
  datasources <- .get_datasources(datasources)
  .verify_datasources(datasources)
  return(datasources)
}

#' Check That a Data Frame Name Is Provided
#'
#' Internal helper that checks whether a data frame or matrix object
#' has been provided. If `NULL`, it aborts with a user-friendly error.
#'
#' @param df A data.frame or matrix.
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @noRd
.check_df_name_provided <- function(df) {
  if(is.null(df)){
    cli_abort("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }
}
