#'
#' @title Returns server-side messages to the client-side  
#' @description This function allows for error messages arising from the
#' running of a server-side assign function to be returned to the client-side. 
#' @details Errors arising from aggregate server-side functions can be returned
#' directly to the client-side. But this is not possible for server-side assign
#' functions because they are designed specifically to write objects to the
#' server-side and to return no meaningful information to the client-side.
#' Otherwise, users may be able to use assign functions to return disclosive
#' output to the client-side.
#' 
#' Server-side functions from which error messages are to be made
#' available are designed to be able to write the designated error message to
#' the \code{$serversideMessage} object into the list that is saved on the server-side
#' as the primary output of that function. So only valid server-side functions of
#' DataSHIELD can write a \code{$studysideMessage}. The error message is a string that 
#' cannot exceed a length of \code{nfilter.string} a default of 80 characters.
#' 
#' Server function called: \code{messageDS}
#' @param message.obj.name is a character string specifying  the name of the list that 
#' contains the message. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.message} returns a list object from each study, 
#' containing the message that has been written by
#' DataSHIELD into \code{$studysideMessage}.
#' @author DataSHIELD Development Team
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
#'   #Use a ds.asCharacter assign function to create the message in the server-side
#'   
#'   ds.asCharacter(x.name = "D$LAB_TRIG", 
#'                  newobj = "vector1",
#'                  datasources = connections)
#'                  
#'   #Return the message to the client-side
#'   
#'   ds.message(message.obj.name = "vector1",
#'              datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#' @export
ds.message<-function(message.obj.name=NULL,datasources=NULL){
  .Deprecated()

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # Check if user has provided the name of the studyside list object that holds the required message
  # Also check that it is in character format (in inverted commas)
  if(is.null(message.obj.name) | !is.character(message.obj.name)){
    stop("Please provide the name of the studyside list object that holds the message\n in character format ie: 'object.name' in inverted commas", call.=FALSE)
  }

# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("messageDS", message.obj.name)
  output.message<-DSI::datashield.aggregate(datasources, calltext)

#RETURN COMPLETION INFORMATION TO .GlobalEnv
    message("\nMESSAGES FROM STUDYSIDE SERVERS ARE:-\n")
    return(Message=output.message)
}
#ds.message
