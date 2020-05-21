#' @title Deletes server-side R object
#' @description deletes an R object on the server-side
#' @details This function is similar to the native R function 
#' \code{rm()}.
#' 
#' The fact that it is an aggregate
#' function may be surprising because it modifies an object
#' on the server-side, and would, therefore, be expected to be an assign function.
#' However, as an assign function the last step in running it
#' would be to write the modified object as \code{newobj}. But this would
#' fail because the effect of the function is to delete the object and so
#' it would be impossible to write it anywhere. Please note that although
#' this calls an aggregate function there is no \code{type} argument.
#' 
#' Server function called: \code{rmDS}
#' @param x.name a character string specifying the object to be deleted. 
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified 
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return The \code{ds.rm}  function deletes from the server-side 
#' the specified object.  If this 
#' is successful the message \code{"Object <x.name> successfully deleted"} is returned
#' to the client-side. 
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
#'   #Create an object in the server-side
#'   
#'   ds.assign(toAssign = "D$LAB_TSC",
#'             newobj = "labtsc",
#'             datasources = connections)
#'   
#'   #Delete "labtsc" object from the server-side
#'   
#'   ds.rm(x.name = "labtsc",
#'         datasources = connections)
#'              
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#' @export
ds.rm<-function(x.name=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x.name)){
   stop("Please provide the name of the object to be deleted (eg 'object.name') as the x.name argument", call.=FALSE)
  }

	#make transmittable via parser
    x.name.transmit <- paste(x.name,collapse=",")


  # call the server side function
  #PLEASE NOTE THIS IS - SURPRISINGLY - AN AGGREGATE FUNCTION: see details in header

	calltext <- call("rmDS", x.name.transmit)

	output = DSI::datashield.aggregate(datasources, calltext)

  return(output)
}

#ds.rm
