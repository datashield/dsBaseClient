#'
#' @title Retrieves the class of an object
#' @description This function is similar to R function \code{class}.
#' @details Same as for the R function \code{class}.
#' @param x an R object
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return a character the type of x
#' @author Gaye, A.; Isaeva, J.
#' @seealso \link{ds.exists} to verify if an object is defined (exists) on the server side.
#' @export
#' @examples
#' \dontrun{
#'
#'   # load that contains the login details
#'   data(logindata)
#'
#'   # login and assign all the stored variables
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   conns <- datashield.login(logins=logindata,assign=TRUE)
#'
#'   # Example 1: Get the class of the whole dataset
#'   ds.class(x='D')
#'
#'   # Example 2: Get the class of the variable PM_BMI_CONTINUOUS
#'   ds.class(x='D$LAB_TSC')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.class <- function(x=NULL, datasources=NULL) {

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }

  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)

  cally <- paste0('class(', x, ')')
  output <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(output)

}
