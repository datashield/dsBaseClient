#' 
#' @title Retrieves the dimension of an object
#' @description this function is similar to R function \code{dim}
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a character, the name of R table object, for example a matrix, array or data frame
#' @return for an array, \code{NULL} or a vector of mode \code{integer}
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
#' # Example 1: Get the dimension of the assigned datasets
#' ds.dim(datasources=opals, x='D')
#' 
#' # Example 2: Input has to be either matrix, data frame or an array
#' \dontrun{ ds.dim(datasources=opals, x='D$LAB_TSC') }
#' }
#' 
ds.dim = function(datasources=NULL, x=NULL) {
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(x)){
    message(" ALERT!")
    message(" Please provide a valid matrix-like object")
    stop(" End of process!", call.=FALSE)
  }
  
  cally <- paste0("dim(", x, ")")
  dimensions <- datashield.aggregate(datasources, as.symbol(cally))
  
  return(dimensions)
}