#' @title Lists server-side functions
#' @description Lists all current server-side functions
#' @details Uses \code{\link[DSI]{datashield.methods}} function from \code{DSI} package to list all
#' assign and aggregate functions on the available data repository servers.
#' The only choice of arguments is in \code{datasources}; i.e. which studies to interrogate. 
#' Once the studies have
#' been selected \code{ds.listServersideFunctions} lists all assign functions for all
#' of these studies and then all aggregate functions for all of them.
#' 
#' This function does not call any server-side function. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.listServersideFunctions} returns to the client-side 
#' a list containing all server-side functions separately for each study. 
#' Firstly lists assign and then aggregate functions.
#' @examples
#' \dontrun{
#'  
#'   ## Version 6, for version 5 see Wiki
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
#'   # List server-side functions
#'   
#'   ds.listServersideFunctions(datasources = connections)
#'             
#'   # Clear the Datashield R sessions and logout  
#'   datashield.logout(connections) 
#' }
#' @author DataSHIELD Development Team
#' @export
#' @import DSI
ds.listServersideFunctions<-function(datasources=NULL){
	.Deprecated("DSI::datashield.methods")

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  assign.funs <- DSI::datashield.methods(datasources, 'assign')
  aggregate.funs <- DSI::datashield.methods(datasources, 'aggregate')
  return(list(serverside.assign.functions=assign.funs,
              serverside.aggregate.functions=aggregate.funs))
}

#ds.listServersideFunctions
