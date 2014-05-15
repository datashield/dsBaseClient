#' 
#' @title Retrieves column names of a matrix-like object
#' @description this function is similar to R function \code{colnames}
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a character, the name of dataframe or matrix
#' @return a string containing column names of the given object
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: Get the column names of the assigned datasets
#' ds.colnames(datasources=opals, x='D')
#' 
#' # Example 2: Vectors are wrong input to the colnames function
#' \dontrun{ ds.colnames(datasources=opals, x='D$LAB_TSC')}
#' }
#' 
ds.colnames = function(datasources=NULL, x=NULL) {
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(x)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid matrix-like object\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  cally <- paste0("colnames(", x, ")")
  column_names <- datashield.aggregate(datasources, cally)
  
  return(column_names)
}