#' 
#' @title Converts a server-side R object into a list 
#' @description Coerces an R object into a list.
#' This function is based on the native R function \code{as.list}.
#' @details 
#' 
#' Server function called: \code{asListDS}
#' @param x.name a character string providing the name of the input object to be coerced to 
#' a list.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{aslist.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.asList} returns the R object converted into a list 
#' which is written to the server-side. Also, two validity messages are returned to the
#' client-side indicating the name of the \code{newobj} which has been created in each data 
#' source and if it is in a valid form.
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
#'   # Converting the R object into a List
#'   ds.asList(x.name = "D",
#'   newobj = "D.asList", 
#'   datasources = connections[1]) #only the first Opal server is used ("study1")
#'   ds.class(x = "D.asList", datasources = connections[1])   
#'               
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @author  DataSHIELD Development Team
#' @export
#' 
ds.asList <- function(x.name=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x.name)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  # check if the input object is defined in all the studies
  isDefined(datasources, x.name)

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "aslist.newobj"
  }

  # call the server side function that does the job

  calltext <- call("asListDS", x.name, newobj)

  out.message <- DSI::datashield.aggregate(datasources, calltext)
# print(out.message)

#Don't include assign function completion module as it can print out an unhelpful
#warning message when newobj is a list

}
# ds.asList
