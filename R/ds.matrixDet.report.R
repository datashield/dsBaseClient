#' @title Returns matrix determinant to the client-side
#' @description Calculates the determinant of a square matrix  and returns
#' the result to the client-side
#' @details Calculates and returns to the client-side
#' the determinant of a square matrix on the server-side. 
#' This function is similar to the native R \code{determinant} function.  
#' This operation is only
#' possible if the number of columns and rows of the matrix are the same.
#' 
#' Server function called: \code{matrixDetDS1}
#' 
#' @param M1  a character string specifying the name of the matrix. 
#' @param logarithm logical. If TRUE the logarithm of the modulus of the determinant
#' is calculated. Default FALSE. 
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.matrixDet.report} returns to the client-side
#' the determinant of a matrix that is stored on the server-side.
#' @author DataSHIELD Development Team
#' @examples 
#' \dontrun{
#' 
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
#'   #Create the matrix in the server-side
#'   
#'   ds.rUnif(samp.size = 9,
#'            min = -10.5,
#'            max = 10.5,
#'            newobj = "ss.vector.9",
#'            seed.as.integer = 5575,
#'            force.output.to.k.decimal.places = 0,
#'            datasources = connections)
#'            
#'   ds.matrix(mdata = "ss.vector.9",
#'             from = "serverside.vector",
#'             nrows.scalar = 9,ncols.scalar = 9,
#'             byrow = TRUE,
#'             newobj = "matrix",
#'             datasources = connections)
#'             
#'   #Calculate the determinant of the matrix
#'   
#'   ds.matrixDet.report(M1 = "matrix", 
#'                       logarithm = FALSE, 
#'                       datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#' @export
#'
ds.matrixDet.report<-function(M1=NULL, logarithm=FALSE, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if user has provided the name of matrix representing M1
  if(is.null(M1)){
    return("Error: Please provide the name of the matrix representing M1")
  }

  # if no value or invalid value specified for logarithm, then specify a default
  if(is.null(logarithm))
  {
  logarithm<-FALSE
  }

  if(logarithm!=TRUE)
  {
  logarithm<-FALSE
  }

  # CALL THE MAIN SERVER SIDE AGGREGATE FUNCTION
  calltext <- call("matrixDetDS1", M1, logarithm)
  output<-DSI::datashield.aggregate(datasources, calltext)

  return(list(matrix.determinant=output))
}
#ds.matrixDet.report
