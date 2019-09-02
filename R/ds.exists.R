#'
#' @title Checks if an object is defined on the server side
#' @description this function is similar to R function \code{exists}
#' @details In DataSHIELD it is not possible to see the data sitting on the servers
#' of the collaborating studies. It is only possible to get summaries of objects stored on the
#' server side. It is however important to know if an object is defined (i.e. exists) son the server
#' side. This function checks if an object do really exists on the server side. Further information
#' about the object can be obtained using functions such as \code{ds.class}, \code{length} etc...
#' @param x a character, the name of the object to look for.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return a boolean, TRUE if the object is on the server side and FALSE otherwise
#' @author Gaye, A.
#' @seealso \link{ds.class} to check the type of an object.
#' @export
#' @examples
#' \dontrun{
#'
#'   # load the file that contains the login details
#'   data(logindata)
#'
#'   # login and assign the required variables to R
#'   myvar <- list("LAB_TSC")
#'   conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#'   # assign 'LAB_TSC' in the dataframe D to a new variable 'labtsc'
#'   ds.assign(toAssign='D$LAB_TSC', newobj='labtsc')
#'
#'   # now let us check if the variable 'labtsc' does now 'exist' on the server side
#'   ds.exists(x='labtsc')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.exists <- function(x=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }

  # call the server side function that does the job
  cally <- call("exists", x)
  output <- DSI::datashield.aggregate(datasources, cally)

  return(output)
}
