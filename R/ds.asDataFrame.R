#' @title  Converts a server-side R object into a data frame
#' @description Coerces an R object into a data frame maintaining original
#' class for all columns in data frames.
#' @details This function is based on the native R function \code{data.frame}.
#'
#' Server function called: \code{asDataFrameDS}.
#' @param x.name a character string providing  the name of the input object to be coerced to
#' a data frame
#' @param newobj a character string that provides the name for the output object
#'  that is stored on the data servers. Default \code{asdataframe.newobj}.
#' @param datasources a list of \code{\link{DSConnection-class}}
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.asDataFrame} returns the object converted into a data frame
#' that is written to the server-side.
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
#'   # Converting the R object into a data frame
#'   ds.asDataFrame(x.name = "D",
#'                   newobj = "mat.obj",
#'                   datasources = connections[1]) #only the first server is used ("study1")
#'
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#' @author DataSHIELD Development Team
#' @export
#'
ds.asDataFrame <- function(x.name=NULL, newobj=NULL, datasources=NULL){

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
    newobj <- "asdataframe.newobj"
  }

  # call the server side function that does the job
	calltext <- call("asDataFrameDS", x.name)
	DSI::datashield.assign(datasources, newobj, calltext)

}
