#'
#' @title Return the names of a list object
#' @description Returns the names of a designated server-side list
#' @details ds.names calls aggregate function namesDS. This function is similar to
#' the native R function \code{names} but it does not subsume all functionality,
#' for example, it only works to extract names that already exist,
#' not to create new names for objects. The function is restricted to objects of
#' type list, but this includes objects that have a primary class other than list but which
#' return TRUE to the native R function \code{is.list}. As an example this includes
#' the multi-component object created by fitting a generalized linear model
#' using ds.glmSLMA. The resultant object saved on each server separately
#' is formally of class "glm" and "ls" but responds TRUE to is.list(),
#' @param xname a character string specifying the name of the list.
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}}
#' objects obtained after login that represent the particular data sources
#' (studies) to be addressed by the function call. If the \code{datasources}
#' argument is not specified the default set of connections will be used:
#' see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.names} returns to the client-side the names
#' of a list object stored on the server-side.
#' @author Amadou Gaye, updated by Paul Burton for DataSHIELD development
#' team 25/06/2020
#' @export
#' @examples
#' \dontrun{
#'
#'  ## Version 6, for version 5 see the Wiki
#'  
#'   # connecting to the Opal servers
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
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'  
#'   #Create a list in the server-side
#'  
#'   ds.asList(x.name = "D",
#'             newobj = "D.list",
#'             datasources = connections)
#'            
#'   #Get the names of the list
#'  
#'   ds.names(xname = "D.list",
#'            datasources = connections)
#'  
#'  
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#'
ds.names <- function(xname=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(xname)){
    stop("Please provide the name of the input list!", call.=FALSE)
  }
  
  # check if the input object is defined in all the studies
  isDefined(datasources, xname)

  calltext <- call("namesDS", xname)
  output <- datashield.aggregate(datasources, calltext)
  return(output)
}
#ds.names
