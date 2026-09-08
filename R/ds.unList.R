#' @title Flattens Server-Side Lists
#' @description Coerces an object of list class back to the class it was when
#' it was coerced into a list. 
#' @details This function is similar to the native R function \code{unlist}.
#' 
#' When an object is coerced to a list, depending
#' on the class of the original object some information may be lost. Thus,
#' for example, when a data frame is coerced to list the information that
#' underpins the structure of the data frame is lost and when it is
#' subject to the function \code{ds.unList} it is returned to a simpler
#' class than data frame e.g. numeric (basically a numeric vector
#' containing all of the original data in all variables in the data frame
#' but with no structure). If you wish to reconstruct the original
#' data frame you, therefore, need to specify this structure again e.g.
#' the column names, etc. 
#' 
#' Server function called: \code{unListDS}
#' @param x.name a character string specifying the name of the input object to be unlisted.
#' @param newobj a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{unlist.newobj}. 
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.unList} returns to the server-side the unlist object.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
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
#'   #Create a list on the server-side
#'   
#'   ds.asList(x.name = "D", 
#'             newobj = "list.D",
#'             datasources = connections)
#'   
#'   #Flatten a server-side lists
#'   
#'   ds.unList(x.name = "list.D",
#'             newobj = "un.list.D",
#'            datasources = connections)
#'  
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @export
ds.unList <- function(x.name=NULL, newobj=NULL, datasources=NULL){

  datasources <- .set_datasources(datasources)

  if(is.null(x.name)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }


  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "unlist.newobj"
  }

     # call the server side function
  calltext <- call("unListDS", x.name)
  DSI::datashield.assign(datasources, newobj, calltext)

}
#ds.unList

