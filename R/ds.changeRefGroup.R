#'
#' @title Changes the reference level of a factor in the server-side
#' @description Change the reference level of a factor, by putting 
#' the reference group first.  
#' 
#' This function is similar to R function \code{relevel}.
#' @details This function
#' allows the user to re-order the vector, putting the reference
#' group first. It should be mentioned that by default the reference is 
#' the first level in the vector of levels.  
#' If the user chooses the re-order a warning is issued
#' as this can introduce a mismatch of values if the vector is put back
#' into a table that is not reordered in the same way. Such mismatch
#' can render the results of operations on that table invalid.
#' 
#' Server function called: \code{changeRefGroupDS}
#' 
#' @param x a character string providing the name of the input vector of type factor.
#' @param ref the reference level.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the server-side. Default \code{changerefgroup.newobj}.
#' @param reorderByRef logical, if TRUE the new vector
#' should be ordered by the reference group (i.e. putting the reference group first).
#' The default is to not re-order (see the reasons in the details). 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.changeRefGroup} returns a new vector  with the specified level as a reference
#' which is written to the server-side. 
#' @author DataSHIELD Development Team
#' @seealso \code{\link{ds.cbind}} Combines objects column-wise.
#' @seealso \code{\link{ds.levels}} to obtain the levels (categories) of a vector of type factor.
#' @seealso \code{\link{ds.colnames}} to obtain the column names of a matrix or a data frame
#' @seealso \code{\link{ds.asMatrix}} to coerce an object into a matrix type.
#' @seealso \code{\link{ds.dim}} to obtain the dimensions of a matrix or a data frame.
#'
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
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
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#' 
#'   # Changing the reference group in the server-side
#'  
#'     # Example 1: rename the categories and change the reference with re-ordering
#'       # print out the levels of the initial vector
#'       ds.levels(x= "D$PM_BMI_CATEGORICAL",
#'                 datasources = connections)
#'
#'       # define a vector with the new levels and recode the initial levels
#'       newNames <- c("normal", "overweight", "obesity")
#'       ds.recodeLevels(x = "D$PM_BMI_CATEGORICAL",
#'                       newCategories = newNames,
#'                       newobj = "bmi_new",
#'                       datasources = connections)
#'
#'       # print out the levels of the new vector
#'       ds.levels(x = "bmi_new",
#'                 datasources = connections)
#'
#'       # Set the reference to "obesity" without changing the order (default)
#'       ds.changeRefGroup(x = "bmi_new",
#'                         ref = "obesity",
#'                         newobj = "bmi_ob",
#'                         datasources = connections)
#'
#'       # print out the levels; the first listed level (i.e. the reference) is now 'obesity'
#'       ds.levels(x = "bmi_ob",
#'                 datasources = connections)
#'
#'     # Example 2: change the reference and re-order by the reference level
#'       # If re-ordering is sought, the action is completed but a warning is issued
#'       ds.recodeLevels(x = "D$PM_BMI_CATEGORICAL",
#'                       newCategories = newNames,
#'                       newobj = "bmi_new",
#'                      datasources = connections)
#'       ds.changeRefGroup(x = "bmi_new",
#'                         ref = "obesity",
#'                         newobj = "bmi_ob",
#'                         reorderByRef = TRUE,
#'                         datasources = connections)
#'
#'            
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @export
ds.changeRefGroup <- function(x=NULL, ref=NULL, newobj=NULL, reorderByRef=FALSE, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of a vector of type factor!", call.=FALSE)
  }

  if(is.null(ref)){
    stop(" You must indicate a reference level - set the parameter 'ref'.", call.=FALSE)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "changerefgroup.newobj"
  }

  # check if the input object is defined in all the studies
  isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # if input vector is not a factor stop
  if(!('factor' %in% typ)){
    stop("The input vector must be a factor!", call.=FALSE)
  }

  if(reorderByRef){
    warning("'reorderByRef' is set to TRUE. Please read the documentation for possible consequences!", call.=FALSE)
  }

  # call the server side function that will recode the levels
  cally <- paste0('changeRefGroupDS(', x, ",'", ref, "',", reorderByRef,")")
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
