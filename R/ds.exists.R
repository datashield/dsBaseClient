#' 
#' @title Checks if an object is defined on the server side
#' @description In DataSHIELD it is not possible to see the data sitting on the servers
#' of the collaborating studies. It is only possible to get summaries of objects stored on the 
#' server side. It is however important to know if an object created on the server is really
#' there. This function checks if an object do really exist on the server side. Further information
#' about the object can also be obtain using function such as \code{ds.class}, \code{length} etc...
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xname a character, the name of the object to look for.
#' @return a bollean, TRUE if the object is on the server side and FALSE otherwise
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # assign 'LAB_TSC' in the dataframe D to a new variable 'labtsc'
#' ds.assign(opals, 'labtsc', 'D$LAB_TSC')
#'
#' # now let us check if the variable 'labtsc' do now 'exist' on the server side
#' ds.exists(datasources=opals, xname='labtsc')
#' }
#' 
ds.exists <- function(datasources=NULL, xname=NULL){
  
  if(is.null(datasources)){
    message("ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(xname)){
    message("\n ALERT!\n")
    message(" Please provide the name of the object to look for.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # call the server side function that does the job
  cally <- call("exists", xname)
  output <- datashield.aggregate(datasources, cally)
  
  return(output)
}
