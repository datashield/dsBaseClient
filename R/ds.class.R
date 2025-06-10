#'
#' @title Class of the R object in the server-side
#' @description Retrieves the class of an R object.
#'  This function is similar to the R function \code{class}.
#' @details Same as the native R function \code{class}.
#' 
#' Server function called: \code{classDS}
#' @param x a character string providing the name of the input R object.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.class} returns the type of the R object. 
#' @author DataSHIELD Development Team
#' @seealso \code{\link{ds.exists}} to verify if an object is defined (exists) on the server-side.
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
#'   # Getting the class of the R objects stored in the server-side
#'   ds.class(x = "D", #whole dataset
#'            datasources = connections[1]) #only the first server ("study1") is used
#'
#'   ds.class(x = "D$LAB_TSC", #select a variable
#'            datasources = connections[1]) #only the first server ("study1") is used
#'            
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @export
#'
ds.class <- function(x=NULL, datasources=NULL) {

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }

  # check if the input object is defined in all the studies
  defined <- isDefined(datasources, x)

  cally <- call('classDS', x)
  output <- DSI::datashield.aggregate(datasources, cally)

  return(output)

}
