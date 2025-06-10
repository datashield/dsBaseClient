#'
#' @title Checks if a server-side object is valid
#' @description Checks if a vector or table structure has a number of observations equal to or greater
#' than the threshold set by DataSHIELD.
#' @details In DataSHIELD, analyses are possible only on valid objects to ensure the output is not disclosive.
#' This function checks if an input object is valid. A vector is valid if the
#' number of observations is equal to or greater than a set threshold. A factor vector is valid if all
#' its levels (categories) have a count equal or greater than the set threshold. A data frame or a matrix
#' is valid if the number of rows is equal or greater than the set threshold.
#' 
#' Server function called: \code{isValidDS}
#' @param x a character string specifying the name of a vector, dataframe or matrix.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.isValid} returns a boolean. If it is TRUE input object is valid, FALSE otherwise. 
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
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
#'   # Check if the dataframe assigned above is valid
#'   ds.isValid(x = 'D',
#'              datasources = connections) #all servers are used
#'   ds.isValid(x = 'D',
#'              datasources = connections[2]) #only the second server is used (study2)
#'  
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#'
ds.isValid <- function(x=NULL, datasources=NULL){

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

  # the input object must be a vector
  if(!('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ) & !('numeric' %in% typ) & !('data.frame' %in% typ) & !('matrix' %in% typ)){
    stop("The input object must be a character, factor, integer, logical or numeric vector or a dataframe or a matrix", call.=FALSE)
  }

  # call the server side function that does the job and return its output
  cally <- paste0('isValidDS(', x, ')')
  output <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  return(output)
}
