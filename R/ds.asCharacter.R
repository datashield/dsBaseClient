#' 
#' @title Converts a server-side R object into a character class
#' @description Converts the input object into a character class. 
#' This function is based on the native R function \code{as.character}.
#' @details 
#' Server function called: \code{asCharacterDS}
#' 
#' @param x.name a character string providing  the name of the input object to be coerced to class
#' character.
#' @param newobj a character string that provides the name for the output object
#'  that is stored on the data servers. Default \code{ascharacter.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.asCharacter} returns the object converted into a class character 
#' that is written to the server-side.
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
#'   # Converting the R object into a class character
#'   ds.asCharacter(x.name = "D$LAB_TSC",
#'                  newobj = "char.obj",
#'                  datasources = connections[1]) #only the first Opal server is used ("study1")
#'                  
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @author DataSHIELD Development Team
#' @export
#' 
ds.asCharacter <- function(x.name=NULL, newobj=NULL, datasources=NULL){

  datasources <- .set_datasources(datasources)

  if(is.null(x.name)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- "ascharacter.newobj"
  }

	calltext <- call("asCharacterDS", x.name)
	DSI::datashield.assign(datasources, newobj, calltext)

}
