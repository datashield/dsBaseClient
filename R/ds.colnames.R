#' 
#' @title Retrieves column names of a matrix-like object
#' @description this function is similar to R function \code{colnames}.
#' @details The input is restricted to object of type 'data.frame' or 'matrix'
#' @param x a character, the name of a dataframe or matrix.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
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
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Get the column names of the assigned datasets (default name is 'D')
#'   ds.colnames(x='D')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#'
#' }
#' 
ds.colnames <- function(x=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
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
  column_names <- opal::datashield.aggregate(datasources, cally)
  
  return(column_names)

}
