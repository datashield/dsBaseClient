#'
#' @title Produces levels attributes of a server-side factor
#' @description This function provides access to the level attribute of
#' a factor variable stored on the server-side. 
#' This function is similar to R function \code{levels}. 
#' @details 
#' Server function called: \code{levelsDS}
#' @param  x a character string specifying  the name of a factor variable. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.levels} returns to the client-side the levels of a factor 
#' class variable stored in the server-side. 
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
#'   # Example 1: Get the levels of the PM_BMI_CATEGORICAL variable
#'   ds.levels(x = 'D$PM_BMI_CATEGORICAL',
#'             datasources = connections)#all servers are used
#'   ds.levels(x = 'D$PM_BMI_CATEGORICAL',
#'             datasources = connections[2])#only the second server is used (study2)
#'
#'   # Example 2: Get the levels of the LAB_TSC variable
#'   # This example should not work because LAB_TSC is a continuous variable
#'   ds.levels(x = 'D$LAB_TSC',
#'             datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#'
ds.levels <- function(x=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  # check if the input object is defined in all the studies
  isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a factor
  if(!('factor' %in% typ)){
    stop("The input object must be a factor.", call.=FALSE)
  }

  # call the server-side function
  cally <- paste0("levelsDS(", x, ")")
  output <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(output)
  
}
