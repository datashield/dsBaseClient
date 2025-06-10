#'
#' @title Replaces the missing values in a server-side vector
#' @description This function identifies missing values and replaces them by a value or
#' values specified by the analyst.
#' @details This function is used when the analyst prefers or requires complete vectors.
#' It is then possible the specify one value for each missing value by first returning
#' the number of missing values using the function \code{ds.numNA} but in most cases,
#' it might be more sensible to replace all missing values by one specific value e.g.
#' replace all missing values in a vector by the mean or median value. Once the missing
#' values have been replaced a new vector is created.
#' 
#' \strong{Note}: If the vector is within a table structure such as a data frame the new vector is
#' appended to table structure so that the table holds both the vector with and without
#' missing values. 
#' 
#' Server function called: \code{replaceNaDS}
#' @param x a character string specifying the name of the vector. 
#' @param forNA a list or a vector that contains the replacement value(s), for each study. 
#' The length of the list or vector must be equal to the number of servers (studies). 
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{replacena.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.replaceNA} returns to the server-side a new vector or table structure 
#' with the missing values replaced by the specified values.
#'  The class of the vector is the same as the initial vector. 
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
#'   # Connecting to the Opal servers
#' 
#'     require('DSI')
#'     require('DSOpal')
#'     require('dsBaseClient')
#'
#'     builder <- DSI::newDSLoginBuilder()
#'     builder$append(server = "study1", 
#'                    url = "http://192.168.56.100:8080/", 
#'                    user = "administrator", password = "datashield_test&", 
#'                    table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'     builder$append(server = "study2", 
#'                    url = "http://192.168.56.100:8080/", 
#'                    user = "administrator", password = "datashield_test&", 
#'                    table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'     builder$append(server = "study3",
#'                    url = "http://192.168.56.100:8080/", 
#'                    user = "administrator", password = "datashield_test&", 
#'                    table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'     logindata <- builder$build()
#' 
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#' 
#'   # Example 1: Replace missing values in variable 'LAB_HDL' by the mean value 
#'   # in each study
#'   
#'   # Get the mean value of  'LAB_HDL' for each study
#'   mean <- ds.mean(x = "D$LAB_HDL",
#'                   type = "split",
#'                   datasources = connections)
#'
#'   # Replace the missing values using the mean for each study
#'   ds.replaceNA(x = "D$LAB_HDL",
#'                forNA = list(mean[[1]][1], mean[[1]][2], mean[[1]][3]),
#'                newobj = "HDL.noNA",
#'                datasources = connections)
#'                
#'   # Example 2: Replace missing values in categorical variable 'PM_BMI_CATEGORICAL'
#'   # with 999s
#'  
#'   # First check how many NAs there are in 'PM_BMI_CATEGORICAL' in each study
#'   ds.table(rvar = "D$PM_BMI_CATEGORICAL", 
#'           useNA = "always")   
#'           
#'   # Replace the missing values with 999s
#'   ds.replaceNA(x = "D$PM_BMI_CATEGORICAL", 
#'                forNA = c(999,999,999), 
#'                newobj = "bmi999")
#'                
#'   # Check if the NAs have been replaced correctly
#'   ds.table(rvar = "bmi999", 
#'           useNA = "always")   
#'  
#'   # Clear the Datashield R sessions and logout  
#'   datashield.logout(connections) 
#' } 
#' 
ds.replaceNA <- function(x=NULL, forNA=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of a vector!", call.=FALSE)
  }
  
  # check if the input object is defined in all the studies
  isDefined(datasources, x)

  # check if replacement values have been provided
  if(is.null(forNA)){
    stop("Please provide a list of replacement values!", call.=FALSE)
  }else{
    if(length(forNA) != length(datasources)){
      stop("'forNA' must be of the same length as the number of datasources/studies!", call.=FALSE)
    }
  }

  for(i in 1:length(datasources)){
    # get the number of missing values for each study and if the number of
    # replacement values is not 1 and is greater or smaller than the actual
    # number of missing values stop the process and tell the analyst
    cally <- call("numNaDS", x)
    numNAs <- DSI::datashield.aggregate(datasources[i], cally)
    if(length(forNA[[i]]) != 1 & length(forNA[[i]]) != numNAs[[1]]){
      message("The number of replacement values must be of length 1 or of the same length as the number of missing values.")
      stop(paste0("This is not the case in ", names(datasources)[i]), call.=FALSE)
    }
  }

  if(is.null(newobj)){
    newobj <- "replacena.newobj"
  }

  # call the server side function and doo the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Processing ", names(datasources)[i], "..."))
    cally <- paste0("replaceNaDS(", x, paste0(", vectorDS(",paste(forNA[[i]],collapse=","),")"), ")")
    DSI::datashield.assign(datasources[i], newobj, as.symbol(cally))

    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj)

    # if the input vector is within a table structure append the new vector to that table
    inputElts <- extract(x)
    if(!(is.na(inputElts[[1]]))){
      ds.dataFrame(c(inputElts[[1]], newobj), newobj = inputElts[[1]], DataSHIELD.checks = FALSE, datasources = datasources[i])
    }
  }

}
