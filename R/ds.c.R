#'
#' @title Combines values into a vector or list in the server-side
#' @description Concatenates objects into one vector.
#' @details To avoid combining the character names and not the 
#' vectors on the client-side, the names are coerced into a list 
#' and the server-side function loops through that list to 
#' concatenate the list's elements into a vector.
#' 
#' Server function called: \code{cDS}
#' @param x  a vector of character string providing the names of the objects to be combined.
#' @param newobj a character string that provides the name for the output object 
#' that is stored on the data servers. Default \code{c.newobj}.
#' @param classConsistencyCheck logical. If TRUE, verifies that each input object has
#' the same class across all studies before concatenation. Default TRUE.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}}
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return  \code{ds.c} returns the vector of concatenating R
#'  objects which are written to the server-side.
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
#'   # Create a vector with combined objects
#'   myvect <- c("D$LAB_TSC", "D$LAB_HDL")
#'   ds.c(x = myvect,
#'        newobj = "new.vect",
#'        datasources = connections[1]) #only the first Opal server is used ("study1")
#'                 
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }    
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
ds.c <- function(x=NULL, newobj=NULL, datasources=NULL, classConsistencyCheck=TRUE){

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("x=NULL. Please provide the names of the objects to concatenate!", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "c.newobj"
  }

  if(classConsistencyCheck){
    for(i in seq_along(x)){
      checkClass(datasources, x[i])
    }
  }

  # call the server side function that does the job
  cally <- call("cDS", x)
  DSI::datashield.assign(datasources, newobj, cally)

}
