#'
#' @title Computes logarithms, by default natural logarithms
#' @description This function is similar to R function \code{log}.
#' @details this is simply a wrapper for the server side function.
#' @param x a vector.
#' @param base a numrical, the base with respect to which logarithms are computed.
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new 
#' variable is the name 'log.newobj'
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples
#' \dontrun{
#'
#'   # load that contains the login details
#'   data(logindata)
#'
#'   # login and assign specific variable(s)
#'   myvar <- list("LAB_TSC")
#'   conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#'   # Compute natural logarithm of LAB_TSC
#'   ds.log(x='D$LAB_TSC')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.log = function(x=NULL, base=exp(1), newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders

  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a vector
  if(!('integer' %in% typ) & !('numeric' %in% typ)){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "log.newobj"
  }

  # call the server side function that does the job
  cally <- paste0("log(", x, ",", base, ")")
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
