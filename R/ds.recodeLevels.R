#'
#' @title Recodes the levels of a server-side factor vector 
#' @description The function replaces the levels of a factor by the specified new ones.
#' @details This function is similar to native R function \code{levels()}. 
#' 
#' It can for example be used to merge two classes into one, to add a level(s) to a vector
#' or to rename (i.e. re-label) the levels of a vector.
#' 
#' Server function called: \code{levels()}

#' @param x  a character string specifying  the name of a factor variable.
#' @param newCategories a character vector specifying the new levels. Its length must  be equal or greater
#' to the current number of levels.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{recodelevels.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.recodeLevels} returns to the server-side a variable of type factor
#' with the replaces levels. 
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
#'   # Recode the levels of a factor variable
#'   
#'   ds.recodeLevels(x = "D$PM_BMI_CATEGORICAL",
#'                   newCategories = c("1","2","3"),
#'                   newobj = "BMI_CAT",
#'                   datasources = connections)
#'                  
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#'
ds.recodeLevels <- function(x=NULL, newCategories=NULL, newobj=NULL, datasources=NULL){
  .Deprecated("ds.recodeValues")

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    message(" ALERT!")
    message(" Please provide a valid numeric of character vector")
    stop(" End of process!", call.=FALSE)
  }

  if(is.null(newCategories)){
    message(" ALERT!")
    message(" Please specify the new categories to recode to")
    stop(" End of process!", call.=FALSE)
  }

  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources,x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # if input vector is not a factor stop
  if(!('factor' %in% typ)){
    stop("The input vector must be a factor!", call.=FALSE)
  }

  # get the current number of levels
  cally <- paste0("levelsDS(", x, ")")
  xx <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  all.study.levels <- c()
  for (study.levels in xx) {
    if (any(is.na(study.levels$Levels)))
       stop(paste0("Failed to get levels from study: ", study.levels$ValidityMessage), call.=FALSE)
    all.study.levels <- c(all.study.levels, study.levels$Levels)
  }
  if(length(unique(all.study.levels)) > length(newCategories)){
    stop("The number of levels you specified is smaller than the levels of the input vector!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames[length(xnames)]

  # if no name was provided for the new variable give it a default name
  if(is.null(newobj)){
    newobj <- paste(varname, "_new", sep="")
  }

  # get the names and number of the studies/datasources
  stdnames <- names(datasources)
  numstudies <- length(stdnames)

  # do the business
  cally <- paste0("recodeLevelsDS(", x, ", vectorDS(","'",paste(newCategories,collapse="','"),"')",")")
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
}
