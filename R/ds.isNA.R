#'
#' @title Checks if a server-side vector is empty
#' @description this function is similar to R function \code{is.na} but instead of a vector
#' of booleans it returns just one boolean to tell if all the elements are missing values.
#' @details In certain analyses such as GLM none of the variables should be missing at complete
#' (i.e. missing value for each observation). Since in DataSHIELD it is not possible to see the data
#' it is important to know whether or not a vector is empty to proceed accordingly.
#' 
#' Server function called: \code{isNaDS}
#' @param x a character string specifying the name of the vector to check.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.isNA} returns a boolean. If it is TRUE the vector is empty 
#' (all values are NA), FALSE otherwise.
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
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
#'   # check if all the observation of the variable 'LAB_HDL' are missing (NA)
#'   ds.isNA(x = 'D$LAB_HDL',
#'           datasources = connections) #all servers are used
#'   ds.isNA(x = 'D$LAB_HDL',
#'           datasources = connections[1]) #only the first server is used (study1) 
#'  
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#' 
ds.isNA <- function(x=NULL, datasources=NULL){

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
  if(!('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ) & !('numeric' %in% typ) & !('data.frame' %in% typ) & !('matrix' %in% typ)){
    stop("The input object must be a character, factor, integer, logical or numeric vector.", call.=FALSE)
  }

  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # name of the variable
  xnames <- extract(x)
  varname <- xnames$elements

  # keep of the results of the checks for each study
  track <- list()

  # call server side function 'isNaDS' to check, in each study, if the vector is empty
  for(i in 1: length(datasources)){
    cally <- call("isNaDS", x)
    out <- DSI::datashield.aggregate(datasources[i], cally)
    if(out[[1]]){
      track[[i]] <- TRUE
      message("The variable ", varname, " in ", stdnames[i], " is missing at complete (all values are 'NA').")
    }else{
      track[[i]] <- FALSE
    }
  }
  names(track) <- stdnames
  return(track)
}
