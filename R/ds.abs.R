#'
#' @title Computes the absolute values of a variable
#' @description Computes the absolute values for a specified numeric or integer vector. 
#' This function is similar to R function \code{abs}.
#' @details The function calls the server-side function \code{absDS} that computes the 
#' absolute values of the elements of a numeric or integer vector and assigns a new vector 
#' with those absolute values on the server-side. The name of the new generated vector is 
#' specified by the user through the argument \code{newobj}, otherwise is named by default to
#' \code{abs.newobj}.
#' @param x a character string providing the name of a numeric or an integer vector.
#' @param newobj a character string that provides the name for the output variable
#' that is stored on the data servers. Default name is set to \code{abs.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.abs} assigns a vector for each study that includes the absolute values of
#' the input numeric or integer vector specified in the argument \code{x}. The created vectors
#' are stored in the servers.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
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
#'   # Example 1: Generate a normally distributed variable with zero mean and variance equal
#'   #  to one and then get their absolute values
#'   ds.rNorm(samp.size=100, mean=0, sd=1, newobj='var.norm', datasources=connections)
#'   # check the quantiles
#'   ds.summary(x='var.norm', datasources=connections)
#'   ds.abs(x='var.norm', newobj='var.norm.abs', datasources=connections)
#'   # check now the changes in the quantiles
#'   ds.summary(x='var.norm.abs', datasources=connections)  
#'
#'   # Example 2: Generate a sequence of negative integer numbers from -200 to -100
#'   # and then get their absolute values
#'   ds.seq(FROM.value.char = '-200', TO.value.char = '-100', BY.value.char = '1', 
#'          newobj='negative.integers', datasources=connections)
#'   # check the quantiles
#'   ds.summary(x='negative.integers', datasources=connections)
#'   ds.abs(x='negative.integers', newobj='positive.integers', datasources=connections)
#'   # check now the changes in the quantiles
#'   ds.summary(x='positive.integers', datasources=connections)
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'
#' }
#'
ds.abs <- function(x=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }

  # check if the input object is defined in all the studies
  isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  if(!('numeric' %in% typ) && !('integer' %in% typ)){
    stop("Only objects of type 'numeric' or 'integer' are allowed.", call.=FALSE)
  }

  # create a name by default if the user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "abs.newobj"
  }

  # call the server side function that does the operation
  cally <- call("absDS", x)
  DSI::datashield.assign(datasources, newobj, cally)

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
