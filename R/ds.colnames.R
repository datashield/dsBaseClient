#'
#' @title Retrieves column names of a matrix-like object
#' @description this function is similar to R function \code{colnames}.
#' @details The input is restricted to object of type 'data.frame' or 'matrix'
#' @param x a character, the name of a dataframe or matrix.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return a character vector
#' @author Gaye, A.; Isaeva, J.
#' @seealso \link{ds.dim} to obtain the dimensions of matrix or a data frame.
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
#'   # Get the column names of the assigned datasets (default name is 'D')
#'   ds.colnames(x='D')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.colnames <- function(x=NULL, datasources=NULL) {

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }

  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # if the input object is not a matrix or a dataframe stop
  if(!('data.frame' %in% typ) & !('matrix' %in% typ)){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }

  cally <- paste0("colnames(", x, ")")
  column_names <- DSI::datashield.aggregate(datasources, cally)

  return(column_names)

}
