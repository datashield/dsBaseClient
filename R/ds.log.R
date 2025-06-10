#'
#' @title Computes logarithms in the server-side 
#' @description Computes the logarithms for a specified numeric vector. 
#' This function is similar to the R \code{log} function. by default natural logarithms. 
#' @details Server function called: \code{log}
#' @param x  a character string providing the name of a numerical vector.
#' @param base a positive number, the base for which logarithms are computed.
#' Default \code{exp(1)}.
#' @param newobj a character string that provides the name for the output variable
#'  that is stored on the server-side. Default \code{log.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.log} returns a vector for each study of the transformed values for the numeric vector 
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
#'   # Calculating the log value of the 'PM_BMI_CONTINUOUS' variable
#'   
#'   ds.log(x = "D$PM_BMI_CONTINUOUS",
#'          base = exp(2),
#'          newobj = "log.PM_BMI_CONTINUOUS",
#'          datasources = connections[1]) #only the first Opal server is used (study1)
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'}
#'
ds.log <- function(x=NULL, base=exp(1), newobj=NULL, datasources=NULL){

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
  if(!('integer' %in% typ) & !('numeric' %in% typ)){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "log.newobj"
  }

  # call the server side function that does the job
  cally <- paste0("log(", x, ",", base, ")")
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
