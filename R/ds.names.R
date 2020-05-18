#'
#' @title Gets the names of a server-side list
#' @description Function to get the names of an object that is stored on the server-side. 
#' @details This function is similar to the R function \code{names}.
#' In DataSHIELD the use of this function is restricted to objects of type list.
#' 
#' Server function called: \code{namesDS}
#' @param x a character string specifying the name of the list. 
#' @param datasources  a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @author DataSHIELD Development Team
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
#'   ds.names(x = "D.list",
#'            datasources = connections)
#'  
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#'
ds.names <- function(x=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("Please provide the name of the input list!", call.=FALSE)
  }else{
    defined <- isDefined(datasources, x)
  }

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a list
  if(!('list' %in% typ)){
    stop("The input object must be a list.", call.=FALSE)
  }

  # call the server side function that does the job.
  cally <- paste0('namesDS(', x, ')')
  output <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  return(output)

}
