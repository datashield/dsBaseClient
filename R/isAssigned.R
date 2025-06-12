#'
#' @title Checks an object has been generated on the server side
#' @description This is an internal function.
#' @details After calling an assign function it is important
#' to know whether or not the action has been completed by
#' checking if the output actually exists on the server side.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @param newobj a character, the name the object to look for.
#' @keywords internal
#' @return nothing is return but the process is stopped if
#' the object was not generated in any one server.
#'
isAssigned <- function(datasources=NULL, newobj=NULL){
  cally <- call('exists', newobj)
  qc <- DSI::datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc==TRUE))
  if(length(indx) > 0 & length(indx) < length(datasources)){
    stop("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!", call.=FALSE)
  }
  if(length(indx) == 0){
    stop("The output object has not been generated for any of the studies!", call.=FALSE)
  }
}
