#'
#' @title Assigns an R object to a name in the server-side
#' @description This function assigns a datashield object to a name, hence creating a new object.
#' @details The new object is stored on the server-side.
#' 
#' \code{ds.assign}  causes a remote assignment by using \code{DSI::datashield.assign}. 
#' The \code{toAssign} argument is checked at the server and 
#' assigned the variable called \code{newobj} on the server-side.
#' 
#' @param toAssign a character string providing the object to assign. 
#' @param newobj a character string that provides the name for the output object
#'  that is stored on the data servers. Default \code{assign.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.assign} returns the R object assigned to a name 
#' that is written to the server-side.
#' @author DataSHIELD Development Team
#' @export
#' @examples 
#' \dontrun{
#'   ## Version 6, for version 5 see the Wiki
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
#'   # Assign a variable to a name
#'   ds.assign(toAssign = "D$LAB_TSC",
#'             newobj = "labtsc",
#'             datasources = connections[1]) #only the first Opal server is used ("study1")
#'                 
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#'
ds.assign <- function(toAssign=NULL, newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(toAssign)){
    stop("Please give the name of object to assign or an expression to evaluate and assign.!\n", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "assign.newobj"
  }

  # now do the business
  DSI::datashield.assign(datasources, newobj, as.symbol(toAssign))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
