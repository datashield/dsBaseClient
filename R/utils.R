#' Retrieve datasources if not specified
#'
#' @param datasources An optional list of data sources. If not provided, the function will attempt
#' to find available data sources.
#' @importFrom DSI datashield.connections_find
#' @return A list of data sources.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.getDatasources <- function(datasources) {
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
.verifyDatasources <- function(datasources) {
  isConnectionClass <- vapply(datasources, function(x) inherits(unlist(x), "DSConnection"), logical(1))
  if (!all(isConnectionClass)) {
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
.resolveDatasources <- function(datasources) {
  datasources <- .getDatasources(datasources)
  .verifyDatasources(datasources)
  return(datasources)
}

#' Check cross-study class consistency from a list of server aggregate results
#'
#' Batch-refactored server functions return a list per study that includes one
#' or more class fields. This helper verifies that the chosen field is identical
#' across all studies and aborts if not.
#'
#' @param results A named list of server-side aggregate results, one per study,
#'   each containing the class field named by `field`.
#' @param field The name of the element holding the class. Default `"class"`.
#' @param objectName Optional name of the input object, used to make the error
#'   message specific. If `NULL` a generic message is used.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkClassConsistency <- function(results, field = "class", objectName = NULL) {
  classes <- lapply(results, function(r) r[[field]])
  if (length(unique(lapply(classes, sort))) > 1) {
    subject <- if (is.null(objectName)) "The input object" else paste0("'", objectName, "'")
    cli_abort("{subject} is not of the same class in all studies!")
  }
}

#' Check That a New Object Name Is Valid
#'
#' Internal helper that checks whether the name supplied for a server-side
#' output object is a single character string. If not, it aborts with a
#' user-friendly error.
#'
#' @param newobj A character string naming the object to be created server-side.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkNewobjName <- function(newobj) {
  if (!is.character(newobj) || length(newobj) != 1) {
    cli_abort("'newobj' must be a single character string")
  }
}

#' Set and verify the name of a server-side output object.
#'
#' Applies the function's default name when `newobj` is `NULL`, then checks the
#' result is a single character string. The default must be applied first, since
#' `NULL` is a valid input meaning "use the default".
#'
#' @param newobj A character string naming the object to be created server-side,
#'   or `NULL` to use `default`.
#' @param default The name to use when `newobj` is `NULL`.
#' @return A validated, single character string.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.resolveNewobjName <- function(newobj, default) {
  if (is.null(newobj)) {
    newobj <- default
  }
  .checkNewobjName(newobj)
  return(newobj)
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
.checkDfNameProvided <- function(df) {
  if(is.null(df)){
    cli_abort("Please provide the name of a data.frame or matrix!")
  }
}

#' Expand an Argument to One Value per Study
#'
#' Arguments that may differ between studies can be given either as a single
#' value, used for every study, or as a vector with one value per study.
#'
#' @param values A vector of length 1 or `numStudies`.
#' @param argName The name of the argument, used in the error message.
#' @param numStudies The number of studies being analysed.
#' @importFrom cli cli_abort
#' @return A vector of length `numStudies`.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.expandToStudies <- function(values, argName, numStudies) {
  if (length(values) == 1) {
    return(rep(values, numStudies))
  }
  if (length(values) != numStudies) {
    cli_abort("'{argName}' must be length 1 or one value per study")
  }
  return(values)
}
