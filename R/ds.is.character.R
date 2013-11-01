#' 
#' @title a test of an object being of type 'character'
#' @description this function is similar to R function \code{is.character}
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a vector to be tested
#' @return a logic value (TRUE if xvect is of type "character")
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
#' # Example 1: Test whether LAB_TSC variable is of type "character"
#' ds.is.character(datasources=opals, xvect=quote(D$LAB_TSC))
#' 
#' }
#' 
ds.is.character = function(datasources=NULL, xvect=NULL) {
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid input\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  cally <- call('is.character', xvect )
  character_tests <- datashield.aggregate(datasources, cally)
  
  return(character_tests)
}