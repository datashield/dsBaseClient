#'
#' @title Perform 'unique' on a variable on the server-side
#' @description Perform 'unique', from the 'base' package on a specified variable on the server-side
#' @details Will create a vector or list which has no duplicate values.
#' 
#' Server function called: \code{uniqueDS}
#' @param x.name a character string providing the name of the variable, in the server, to perform \code{unique} upon
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{unique.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return  \code{ds.unique} returns the vector of unique R objects which are written to the server-side.
#' @examples 
#' \dontrun{
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
#'   # Create a vector with combined objects
#'   ds.unique(x.name = "D$LAB_TSC", newobj = "new.vect", datasources = connections)
#'
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#' @author Stuart Wheater, DataSHIELD Development Team
#' @export
#'
ds.unique <- function(x.name = NULL, newobj = NULL, datasources = NULL) {
    # look for DS connections
    if (is.null(datasources)) {
        datasources <- datashield.connections_find()
    }

    # ensure datasources is a list of DSConnection-class
    if (!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))) {
        stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
    }

    if (is.null(x.name)) {
        stop("x.name=NULL. Please provide the names of the objects to de-duplicated!", call. = FALSE)
    }

    # create a name by default if user did not provide a name for the new variable
    if (is.null(newobj)) {
        newobj <- "unique.newobj"
    }

    # call the server side function that does the job
    cally <- call('uniqueDS', x.name)
    DSI::datashield.assign(datasources, newobj, cally)

    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources, newobj)
}
