#' 
#' @title Retrieves the class of an object
#' @description this function is similar to R function \code{class}
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x an R object
#' @return class of x
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
#' # Example 1: Get the class of the whole dataset
#' ds.class(datasources=opals, x=quote(D))
#' 
#' # Example 2: Get the class of the variable PM_BMI_CONTINUOUS
#' ds.class(datasources=opals, x=quote(D$LAB_TSC))
#' }
#' 
ds.class = function(datasources=NULL, x=NULL) {
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(x)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid object\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  cally <- call('class', x )
  classes <- datashield.aggregate(datasources, cally)
  
  return(classes)
}