#' 
#' @title Checks if an variable is valid
#' @details This function checks if an input variable is valid. In DataSHIELD, a numeric variable
#' is not valid if it has > 0 and < 5 observations and a factor variable is not valid if
#' any of its levels (categories) has a count of between 1 and 4. The process is stopped if the input
#' variable is not valid to prevent further processing that might be revealing.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a numeric or factor vector.
#' @return  a list with booleans (one for each study), TRUE if input vector is valid and FALSE otherwise.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: Check if the numerical variable 'LAB_TSC' is valid.
#' ds.isValid(datasources=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Example 2: Check if the factor variable 'GENDER' is valid.
#' ds.isValid(datasources=opals, xvect=quote(D$GENDER))
#' 
#' }
#' 
ds.isValid = function(datasources=NULL, xvect=NULL){
  
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # call the server side function that does the job and return its output
  cally <- call('isValid.ds', xvect )
  output <- datashield.aggregate(datasources, cally)
  return(output)
  
}
