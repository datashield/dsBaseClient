#'
#' @title Ensures that the requested subset is not larger than the original object
#' @description Compares subset and original object sizes and eventually carries out subsetting. 
#' @details This function is called by the function \code{ds.subset} to ensure that the requested subset
#' is not larger than the original object.
#' 
#' This function is internal.
#' 
#' Server function called: \code{dimDS}
#' @param dts a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @param data a character string specifying the name of the data frame or
#' the factor vector and the range of the subset.
#' @param rs a vector of two integers specifying the indices of the rows de extract.
#' @param cs a vector of two integers or one or more characters.
#' @keywords internal
#' @return \code{subsetHelper} returns a message or the class of the object if the 
#' object has the same class in all studies.
#' @export
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
#'   subsetHelper(dts = connections, 
#'                data = "D", 
#'                rs = 1:10, 
#'                cs = c("D$LAB_TSC","D$LAB_TRIG"))  
#'                       
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
subsetHelper <- function(dts, data, rs=NULL, cs=NULL){

  # if the size of the requested subset is greater than that of original inform the user and stop the process
  dims <- DSI::datashield.aggregate(dts, call("dimDS", data))
  fail <- c(0,0)

  if(!(is.null(rs))){
    if(length(rs) > dims[[1]][1] ){
      fail[1] <- 1
    }
  }

  if(!(is.null(cs))){
    if(length(cs) > dims[[1]][2]){
      fail[2] <- 1
    }
  }
  return(fail)

}
