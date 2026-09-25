#'
#' @title Computes rows and columns sums and means in the server-side
#' @description Computes sums and means of rows or columns 
#' of a numeric matrix or data frame on the server-side.
#' @details The function is similar to R base functions \code{rowSums}, \code{colSums},
#' \code{rowMeans} and \code{colMeans} with some restrictions.
#' 
#' The results of the calculation are not returned to the user if they are potentially
#' revealing i.e. if the number of rows is less than the allowed number of observations.
#' 
#' Server functions called: \code{classDS}, \code{dimDS} and \code{colnamesDS} 
#' @param x a character string specifying  the name of a matrix or a data frame.
#' @param operation a character string that indicates the operation to carry out:
#' \code{"rowSums"}, \code{"colSums"}, \code{"rowMeans"} or \code{"colMeans"}.
#' @param newobj a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{rowcolcalc.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.rowColCalc} returns to the server-side  rows and columns sums and means. 
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @examples
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki 
#'   # Connecting to the Armadillo/Opal servers
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
#'   myvar <- list("LAB_TSC","LAB_HDL")
#'    
#'   # Log onto the remote Armadillo/Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, 
#'   variables = myvar, symbol = "D") 
#'
#'   
#'   #Calculate the colSums
#'   
#'   ds.rowColCalc(x = "D",
#'                 operation = "colSums", 
#'                 newobj = "D.rowSums", 
#'                 datasources = connections)
#'                 
#'   #Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'
#' }
#' @export
#' 
ds.rowColCalc <- function(x=NULL, operation=NULL, newobj=NULL, datasources=NULL){

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }

  ops <- c("rowSums","colSums","rowMeans","colMeans")
  if(is.null(operation)){
    message(" ALERT!")
    message(" Please indicate the calculation required.")
    stop("'operation' = NULL. Please set it to 'rowSums', 'colSums', 'rowMeans' or 'colMeans'", call.=FALSE)
  }else{
    if(!any(operation %in% ops)){
      stop("'operation' must be set to: 'rowSums', 'colSums', 'rowMeans' or 'colMeans'")
    }
  }

  # operation to carry out
  indx <- which(ops == operation)

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "rowcolcalc.newobj"
  }

  # call the server side function that does the job
  DSI::datashield.assign(datasources, newobj, call("rowColCalcDS", dataset.name=x, operation=indx))

}
