#' 
#' @title Retrieves the dimension of an object
#' @description this function is similar to R function \code{dim}
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x an R object, for example a matrix, array or data frame
#' @return for an array, \code{NULL} or a vector of mode \code{integer}
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk) and Isaeva, J. (julia.isaeva@fhi.no)
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
#' ds.dim(datasources=opals, x=quote(D))
#' 
#' # Example 2: Input has to be either matrix, data frame or an array
#' ds.dim(datasources=opals, x=quote(D$LAB_TSC))
#' }
#' 
ds.dim = function(datasources=NULL, x=NULL) {
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
  
  #   num.sources=length(datasources)
  #   for (i in 1:num.sources) {
  #     if ( (!is.matrix(x)) && (!is.data.frame(x)) ){
  #       message("\n\n ALERT!\n")
  #       message(" Please provide a valid matrix-like object for study ",i, "\n")
  #       stop(" End of process!\n\n", call.=FALSE)
  #     }
  #   }
  
  
  cally <- call('dim', x )
  dimensions <- datashield.aggregate(datasources, cally)
  
  return(dimensions)
}