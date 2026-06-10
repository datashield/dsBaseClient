#'
#' @title Computes the square root values of a variable
#' @description Computes the square root values for a specified numeric or integer vector. 
#' This function is similar to R function \code{sqrt}.
#' @details The function calls the server-side function \code{sqrtDS} that computes the 
#' square root values of the elements of a numeric or integer vector and assigns a new vector 
#' with those square root values on the server-side. The name of the new generated vector is 
#' specified by the user through the argument \code{newobj}, otherwise is named by default to
#' \code{sqrt.newobj}.
#' @param x a character string providing the name of a numeric or an integer vector.
#' @param newobj a character string that provides the name for the output variable
#' that is stored on the data servers. Default name is set to \code{sqrt.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.sqrt} assigns a vector for each study that includes the square root values of
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
#'   # Example 1: Get the square root of LAB_HDL variable
#'   ds.sqrt(x='D$LAB_HDL', newobj='LAB_HDL.sqrt', datasources=connections)
#'   # compare the mean of LAB_HDL and of LAB_HDL.sqrt
#'   # Note here that the number of missing values is bigger in the LAB_HDL.sqrt 
#    # vector, because the variable LAB_HDL includes negative values which
#    # don't have real square roots and those values are returned as NAs.
#'   ds.mean(x='D$LAB_HDL', datasources=connections)
#'   ds.mean(x='LAB_HDL.sqrt', datasources=connections)
#'
#'   # Example 2: Generate a repeated vector of the squares of integers from 1 to 10
#'   # and get their square roots
#'   ds.make(toAssign='rep((1:10)^2, times=10)', newobj='squares.vector', datasources=connections)
#'   ds.sqrt(x='squares.vector', newobj='sqrt.vector', datasources=connections)
  # check the behaviour of that operation by comparing the tables of squares.vector and sqrt.vector
#'   ds.table(rvar='squares.vector')$output.list$TABLE_rvar.by.study_counts
#'   ds.table(rvar='sqrt.vector')$output.list$TABLE_rvar.by.study_counts
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'
#' }
#'
ds.sqrt <- function(x=NULL, newobj=NULL, datasources=NULL){

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
    newobj <- "sqrt.newobj"
  }

  # call the server side function that does the operation
  cally <- call("sqrtDS", x)
  DSI::datashield.assign(datasources, newobj, cally)

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
