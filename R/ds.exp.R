#'
#' @title Computes the exponentials in the server-side
#' @description Computes the exponential values for a specified numeric vector. 
#' This function is similar to R function \code{exp}.
#' @details 
#' 
#' Server function called: \code{expDS}.
#' 
#' @param x a character string providing the name of a numerical vector.
#' @param newobj a character string that provides the name for the output variable
#' that is stored on the data servers. Default \code{exp.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified 
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.exp} returns a vector for each study of the exponential values for the numeric vector 
#' specified in the argument \code{x}. The created vectors are stored in the server-side. 
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#'   ## Version 6, for version 5 see the Wiki 
#'   # Connecting to the Opal servers
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
#'                  
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # compute exponential function of the 'PM_BMI_CONTINUOUS' variable
#'   ds.exp(x = "D$PM_BMI_CONTINUOUS",
#'          newobj = "exp.PM_BMI_CONTINUOUS",
#'          datasources = connections[1]) #only the first Opal server is used (study1)
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'
#' }
#'
ds.exp <- function(x=NULL, newobj=NULL, datasources=NULL){

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }

  if(is.null(newobj)){
    newobj <- "exp.newobj"
  }

  cally <- call("expDS", x)
  DSI::datashield.assign(datasources, newobj, cally)

}
