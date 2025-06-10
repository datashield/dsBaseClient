#'
#' @title Checks if the objects are defined in all studies
#' @description This is an internal function.
#' @details In DataSHIELD an object included in analysis must be defined (i.e. exists)
#' in all the studies. If not the process should halt.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified, the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @param obj a character vector, the name of the object(s) to look for.
#' @param error.message a Boolean which specifies if the function should stop and return
#' an error message when the input object is not defined in one or more studies or to
#' return a list of TRUE/FALSE indicating in which studies the object is defined
#' @keywords internal
#' @return returns an error message if \code{error.message} argument is set to TRUE (default)
#' and if the input object is not defined in one or more studies, or a Boolean value if
#' \code{error.message} argument is set to FALSE.
#' @author Demetris Avraam for DataSHIELD Development Team
#'
isDefined <- function(datasources=NULL, obj=NULL, error.message=TRUE){

  inputobj <- unlist(obj)
  
  for(i in 1:length(inputobj)){
    
    extractObj <- extract(inputobj[i])
  
    if(is.na(extractObj$holders)){
      cally <- call('exists', extractObj$elements)
      out <- DSI::datashield.aggregate(datasources, cally)
    }else{
      dfname <- as.name(extractObj$holders)
      cally <- call('exists', extractObj$elements, dfname)
      out <- DSI::datashield.aggregate(datasources, cally)
    }
    
    if(error.message==TRUE & any(out==FALSE)){
      stop("The input object ", inputobj[i], " is not defined in ", paste(names(which(out==FALSE)), collapse=", "), "!" , call.=FALSE)
    }else{
      return(out)
    }
  }
}
