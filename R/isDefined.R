#'
#' @title Checks if an object is defined in all studies
#' @description This is an internal function.
#' @details In DataSHIELD an object included in analysis must be defined (i.e. exists)
#' in all the studies. If not the process should halt.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified, the default set of connections will be
#' used: see \code{\link{datashield.connections_default}}.
#' @param obj a character, the name of the object to look for.
#' @keywords internal
#' @return a boolean value.
#'
isDefined <- function(datasources=NULL, obj=NULL){

  extractObj <- extract(obj)

  if(is.na(extractObj$holders)){
    cally <- call('exists', extractObj$elements)
    out <- DSI::datashield.aggregate(datasources, cally)
  }else{
    dfname <- as.name(extractObj$holders)
    cally <- call('exists', extractObj$elements, dfname)
    out <- DSI::datashield.aggregate(datasources, cally)
  }
  
  if(any(out==FALSE)){
    stop("The input object ", obj, " is not defined in one or more of the studies!", call.=FALSE)
  }else{
    return(TRUE)
  }

}
