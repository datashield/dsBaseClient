#'
#' @title Produces column names of the R object in the server-side
#' @description Retrieves column names of an R object on the server-side.  
#' This function is similar to R function \code{colnames}.
#' @details The input is restricted to the object of type \code{data.frame} or \code{matrix}. 
#' 
#' Server function called: \code{colnamesDS}
#' @param x a character string providing the name of the input data frame or matrix.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.colnames} returns the column names of 
#' the specified server-side data frame or matrix. 
#' @author DataSHIELD Development Team
#' @seealso \code{\link{ds.dim}} to obtain the dimensions of a matrix or a data frame.
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
#'   # Connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#' 
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#' 
#'   # Getting column names of the R objects stored in the server-side
#'   ds.colnames(x = "D",
#'               datasources = connections[1]) #only the first server ("study1") is used
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @export
#'
ds.colnames <- function(x=NULL, datasources=NULL) {

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }

  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # if the input object is not a matrix or a dataframe stop
  if(!('data.frame' %in% typ) & !('matrix' %in% typ)){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }

  cally <- call("colnamesDS", x)
  column_names <- DSI::datashield.aggregate(datasources, cally)

  return(column_names)

}
