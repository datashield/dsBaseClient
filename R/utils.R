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
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.verifyDatasources <- function(datasources, call = parent.frame()) {
  isConnectionClass <- vapply(datasources, function(x) inherits(unlist(x), "DSConnection"), logical(1))
  if (!all(isConnectionClass)) {
    cli_abort("{.arg datasources} must be a list of {.cls DSConnection} objects.", call = call)
  }
}

#' Set and verify data sources.
#'
#' @param datasources An optional list of data sources. If not provided, the function will attempt
#' to find available data sources.
#' @param call The frame to report the error from.
#' @return A list of verified data sources.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.resolveDatasources <- function(datasources, call = parent.frame()) {
  datasources <- .getDatasources(datasources)
  .verifyDatasources(datasources, call = call)
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
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkClassConsistency <- function(results, field = "class", objectName = NULL, call = parent.frame()) {
  classes <- lapply(results, function(r) r[[field]])
  if (length(unique(lapply(classes, sort))) > 1) {
    if (is.null(objectName)) {
      cli_abort("The input object must have the same class in all studies.", call = call)
    }
    cli_abort("{.val {objectName}} must have the same class in all studies.", call = call)
  }
}

#' Check That a New Object Name Is Valid
#'
#' Internal helper that checks whether the name supplied for a server-side
#' output object is a single character string. If not, it aborts with a
#' user-friendly error.
#'
#' @param newobj A character string naming the object to be created server-side.
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkNewobjName <- function(newobj, call = parent.frame()) {
  if (!is.character(newobj) || length(newobj) != 1) {
    cli_abort("{.arg newobj} must be a single character string.", call = call)
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
#' @param call The frame to report the error from.
#' @return A validated, single character string.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.resolveNewobjName <- function(newobj, default, call = parent.frame()) {
  if (is.null(newobj)) {
    newobj <- default
  }
  .checkNewobjName(newobj, call = call)
  return(newobj)
}

#' Check That a Required Argument Is Provided
#'
#' Internal helper that aborts if a required argument is `NULL`, naming the
#' argument as it was passed by the calling function.
#'
#' @param x The argument to check.
#' @param description What the argument must be, e.g. "the name of the input vector".
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkArgProvided <- function(x, description, call = parent.frame()) {
  if (is.null(x)) {
    cli_abort("{.arg {deparse(substitute(x))}} must be {description}, not {.code NULL}.", call = call)
  }
}

#' Check That Required Arguments Are All Set
#'
#' @param ... The arguments to check, named as the user knows them.
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkArgsSet <- function(..., call = parent.frame()) {
  values <- list(...)
  if (any(vapply(values, is.null, logical(1)))) {
    cli_abort("{.arg {names(values)}} must all be set.", call = call)
  }
}

#' Convert a Seed to the Text Sent to the Server
#'
#' @param seed.as.integer A number, or `NULL` for no seed.
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @return The seed as a character string, or `"NULL"` when no seed was given.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.seedAsText <- function(seed.as.integer, call = parent.frame()) {
  if (is.null(seed.as.integer)) {
    return("NULL")
  }
  if (!is.numeric(seed.as.integer)) {
    cli_abort("{.arg seed.as.integer} must be an integer or {.code NULL}.", call = call)
  }
  return(as.character(seed.as.integer))
}

#' Warn About an Offset or Weights Written Into a Model Formula
#'
#' @param formula The model formula, as a character string or formula.
#' @param call The frame to report the warning from.
#' @importFrom cli cli_warn
#' @return Invisibly returns `NULL`. Called for its side effect.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.warnOffsetInFormula <- function(formula, call = parent.frame()) {
  if (any(grepl("offset|weights", formula, ignore.case = TRUE))) {
    cli_warn(c(
      "You may have specified an offset or regression weights in the model formula.",
      "i" = "Specify them separately with the {.arg offset} and {.arg weights} arguments."
    ), call = call)
  }
}

#' Check That Each Object Has a Class That Can Be Combined
#'
#' @param datasources A list of data sources.
#' @param x The names of the server-side objects.
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @return Invisibly returns `NULL`. Called for its side effect (error checking).
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkObjectClasses <- function(datasources, x, call = parent.frame()) {
  allowed <- c("data.frame", "matrix", "numeric", "integer", "character", "factor", "logical")
  for (name in x) {
    if (!any(allowed %in% checkClass(datasources, name))) {
      cli_abort("Each object must be of class {.or {.val {allowed}}}.", call = call)
    }
  }
}

#' Check That Objects to Be Bound Together Fit in Every Study
#'
#' Warns if column names are duplicated, and aborts if the objects differ in
#' their number of rows.
#'
#' @param datasources A list of data sources.
#' @param x The names of the server-side objects.
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort cli_warn
#' @return Invisibly returns `NULL`. Called for its side effect.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.checkMultiObjectConsistency <- function(datasources, x, call = parent.frame()) {
  isTable <- vapply(x, function(name) any(checkClass(datasources, name) %in% c("data.frame", "matrix")), logical(1))
  for (j in seq_along(datasources)) {
    study <- datasources[j]
    columns <- lapply(seq_along(x), function(i) {
      if (isTable[[i]]) ds.colnames(x = x[i], datasources = study) else as.character(x[i])
    })
    if (anyDuplicated(unlist(columns)) != 0) {
      cli_warn("Some column names in study {j} are duplicated, so a suffix {.val .k} is added to the kth replicate.", call = call)
    }
    rows <- vapply(seq_along(x), function(i) {
      if (isTable[[i]]) {
        return(as.numeric(ds.dim(x = x[i], type = "split", datasources = study)[[1]][1]))
      }
      as.numeric(ds.length(x[i], type = "split", datasources = study)[[1]])
    }, numeric(1))
    if (any(rows != rows[1])) {
      cli_abort("All components must have the same number of rows.", call = call)
    }
  }
}

#' Abort Because an Argument Is Not One of the Allowed Values
#'
#' @param argName The name of the argument.
#' @param choices A character vector of the allowed values.
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort cli_inform cli_verbatim cli_warn
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.abortInvalidChoice <- function(argName, choices, call = parent.frame()) {
  cli_abort("{.arg {argName}} must be {.or {.val {choices}}}.", call = call)
}

#' Expand an Argument to One Value per Study
#'
#' Arguments that may differ between studies can be given either as a single
#' value, used for every study, or as a vector with one value per study.
#'
#' @param values A vector of length 1 or `numStudies`.
#' @param argName The name of the argument, used in the error message.
#' @param numStudies The number of studies being analysed.
#' @param call The frame to report the error from.
#' @importFrom cli cli_abort
#' @return A vector of length `numStudies`.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @noRd
.expandToStudies <- function(values, argName, numStudies, call = parent.frame()) {
  if (length(values) == 1) {
    return(rep(values, numStudies))
  }
  if (length(values) != numStudies) {
    cli_abort("{.arg {argName}} must have length 1 or one value per study.", call = call)
  }
  return(values)
}
