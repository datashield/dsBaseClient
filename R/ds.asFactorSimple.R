#' 
#' @title Converts a numeric vector into a factor
#' @description ds.asFactorSimple calls the assign function asFactorSimpleDS and
#' thereby coerces a numeric or character vector into a factor 
#' @details The function converts the input variable into a factor. Unlike 
#' ds.asFactor and its serverside functions, ds.asFactorSimple does no more than
#' coerce the class of a variable to make it a factor on the serverside in each data source.
#' It does not check for or enforce consistency of factor levels across sources or allow you to
#' force an arbitrary set of levels unless those levels actually exist in the sources.
#' Furthermore, it does not allow you to create an array of
#' binary dummy variables that is equivalent to a factor. If you need to do any
#' of these things you will have to use the ds.asFactor function.
#' @param input.var.name a character string which provides 
#' the name of the variable to be converted to a factor. 
#' @param newobj.name a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{asfactor.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return an output vector of class factor written to the serverside.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
ds.asFactorSimple <- function(input.var.name=NULL, newobj.name=NULL, datasources=NULL){

  datasources <- .set_datasources(datasources)

  # check if user has provided the name of the column that holds the input variable
  if(is.null(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor e.g. 'varname'", call.=FALSE)
  }

  # check if user has provided the name of the input variable in a correct character format
  if(!is.character(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor in character format e.g. 'varname'", call.=FALSE)
  }

  # if no output variable specified then provide a default name
  if(is.null(newobj.name)){
    newobj.name <- "asfactor.newobj"
  }


#Call the only serverside function required for this simple version of asFactor
  calltext0 <- call("asFactorSimpleDS", input.var.name)
  DSI::datashield.assign(datasources, newobj.name, calltext0)

}
#ds.asFactorSimple
