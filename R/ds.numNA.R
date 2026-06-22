#'
#' @title Gets the number of missing values in a server-side vector
#' @description This function helps to know the number of missing values
#' in a vector that is stored on the server-side. 
#'
#' @details The number of missing entries are counted and the total for each study is returned.
#' 
#' Server function called: \code{numNaDS}
#' @param x a character string specifying the name of the vector.
#' @template classConsistencyCheckTrue
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.numNA} returns to the client-side the number of missing values
#' on a server-side vector. 
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#' @examples
#' \dontrun{
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
#'   #Get the number of missing values on a server-side vector 
#'   
#'   ds.numNA(x = "D$LAB_TSC",
#'            datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#'
#' }
#'
ds.numNA <- function(x=NULL, classConsistencyCheck=TRUE, datasources=NULL){

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("Please provide the name of a vector!", call.=FALSE)
  }

  cally <- call("numNaDS", x)
  results <- DSI::datashield.aggregate(datasources, cally)

  if(classConsistencyCheck){
    .checkClassConsistency(results)
  }

  numNAs <- lapply(results, function(r) r$numNA)
  return(numNAs)
}
