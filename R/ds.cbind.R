#' 
#' @title Combines objects by columns
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param objects a list of objects to combine.
#' @param newobj the name of the output object. If this argument is set to \code{NULL}, 
#' the name of the new object is 'newObject'.
#' @return  a message is displayed when the action is completed.
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC","LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # combine the 'LAB_TSC' and 'LAB_HDL' variables by columns
#' myobjects <- list(D$LAB_TSC, D$LAB_HDL)
#' ds.cbind(datasources=opals, objects=myobjects)
#' }
#' 
ds.cbind = function(datasources=NULL, objects=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(objects)){
    message("\n ALERT!\n")
    message(" Please provide the list objects to combine.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- 'newObject'
  }
  
  # call the server side function
  cally <- call('cbindDS', objs=objects)
  datashield.assign(datasources, newobj, cally)
  
}