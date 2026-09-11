#' @title Specifies the dimnames of the server-side matrix
#' @description Adds the row names, the column names or both to
#' a matrix on the server-side.
#' @details This function is similar to the native R \code{dimnames} function.
#' 
#' Server function called: \code{matrixDimnamesDS}
#' @param M1 a character string specifying
#' the name of a server-side matrix.
#' @param dimnames a list of length 2 giving
#' the row and column names respectively. 
#' An empty list is treated as NULL. 
#' @param newobj a character string that provides the name for the output 
#' variable that is stored on the data servers. Default \code{matrixdimnames.newobj}.
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.matrixDimnames} returns to the server-side
#' the matrix with specified row and column names.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
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
#'             
#'   #Example 1: Set the row and column names of a server-side matrix
#'   
#'   #Create the server-side vector 
#'   
#'   ds.rUnif(samp.size = 9,
#'            min = -10.5,
#'            max = 10.5,
#'            newobj = "ss.vector.9",
#'            seed.as.integer = 5575,
#'            force.output.to.k.decimal.places = 0,
#'            datasources = connections)
#'            
#'   #Create the server-side matrix
#'            
#'   ds.matrix(mdata = "ss.vector.9",
#'             from = "serverside.vector",
#'             nrows.scalar = 3,
#'             ncols.scalar = 4,
#'             byrow = TRUE,
#'             newobj = "matrix",
#'             datasources = connections)
#'    
#'   #Specify the column and row names of the matrix
#'   
#'   ds.matrixDimnames(M1 = "matrix",
#'                     dimnames = list(c("a","b","c"),c("a","b","c","d")),
#'                     newobj = "matrix.dimnames",
#'                     datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#' @export

ds.matrixDimnames<-function(M1=NULL, dimnames=NULL, newobj=NULL, datasources=NULL){

  datasources <- .set_datasources(datasources)

  # check if user has provided the name of matrix representing M1
  if(is.null(M1)){
    return("Error: Please provide the name of the matrix representing M1")
  }

  # if no value specified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- "matrixdimnames.newobj"
  }



# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("matrixDimnamesDS", M1, dimnames)
  DSI::datashield.assign(datasources, newobj, calltext)

}
#ds.matrixDimnames
