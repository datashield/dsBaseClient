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
#' @template classConsistencyCheckTrue
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}}
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.isNA} returns a boolean. If it is TRUE the vector is empty
#' (all values are NA), FALSE otherwise.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
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
ds.isNA <- function(x=NULL, datasources=NULL, classConsistencyCheck=TRUE){

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  stdnames <- names(datasources)
  xnames <- extract(x)
  varname <- xnames$elements

  cally <- call("isNaDS", x)
  results <- DSI::datashield.aggregate(datasources, cally)

  if(classConsistencyCheck){
    .checkClassConsistency(results)
  }

  # report per-study if all NA
  track <- list()
  for(i in 1:length(results)){
    if(results[[i]]$is.na){
      track[[i]] <- TRUE
      message("The variable ", varname, " in ", stdnames[i], " is missing at complete (all values are 'NA').")
    }else{
      track[[i]] <- FALSE
    }
  }
  names(track) <- stdnames
  return(track)
}
