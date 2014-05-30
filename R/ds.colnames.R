#' 
#' @title Retrieves column names of a matrix-like object
#' @description this function is similar to R function \code{colnames}
#' @param x a character, the name of a dataframe or matrix.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a character vector
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign all the variables
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Get the column names of the assigned datasets (default name is 'D')
#' ds.colnames(x='D')
#'
#' }
#' 
ds.colnames = function(x=NULL, datasources=NULL) {
  
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        stop(" Please set the parameter 'datasources' to the list you want to use. ", call.=FALSE)
      }
    }
  }
  
  if(is.null(x)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid object\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # if the input object is not a matrix or a dataframe stop
  if(typ != 'data.frame' & typ != 'matrix'){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }
  
  cally <- paste0("colnames(", x, ")")
  column_names <- datashield.aggregate(datasources, cally)
  
  return(column_names)
}