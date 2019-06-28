#' 
#' @title Checks if an object is defined on the server side
#' @description this function is similar to R function \code{exists}
#' @details In DataSHIELD it is not possible to see the data sitting on the servers
#' of the collaborating studies. It is only possible to get summaries of objects stored on the 
#' server side. It is however important to know if an object is defined (i.e. exists) son the server
#' side. This function checks if an object do really exists on the server side. Further information
#' about the object can be obtained using functions such as \code{ds.class}, \code{length} etc...
#' @param x a character, the name of the object to look for.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a boolean, TRUE if the object is on the server side and FALSE otherwise
#' @author Gaye, A.
#' @seealso \link{ds.class} to check the type of an object.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login and assign the required variables to R
#'   myvar <- list("LAB_TSC")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # assign 'LAB_TSC' in the dataframe D to a new variable 'labtsc'
#'   ds.assign(toAssign='D$LAB_TSC', newobj='labtsc')
#'
#'   # now let us check if the variable 'labtsc' does now 'exist' on the server side
#'   ds.exists(x='labtsc')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.exists <- function(x=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  # call the server side function that does the job
  cally <- call("exists", x)
  output <- opal::datashield.aggregate(datasources, cally)
  
  return(output)
}
