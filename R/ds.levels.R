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
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
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

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  cally <- call("levelsDS", x)
  results <- DSI::datashield.aggregate(datasources, cally)

  output <- lapply(results, function(r) list(Levels = r$Levels))
  return(output)

}
