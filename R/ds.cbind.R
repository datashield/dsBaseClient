#' 
#' @title Combines objects by columns
#' @description this is similar to the R base function 'cbind'
#' @details the function combines vectors or vectors and matrices or dataframes by columns.
#' But unlike the R base function 'cbind' the output is a dataframe.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param objects a list which holds the names of the objects to combine
#' @param newobj the name of the output object. If this argument is set to \code{NULL}, 
#' the name of the new object is 'newObject'.
#' @return  a message is displayed when the action is completed.
#' @author Gaye, A.
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
#' # generate a new dataframe by combining the variables 'LAB_TSC' and 'LAB_HDL', by columns
#' ds.assign(opals, 'labtsc', 'D$LAB_TSC')
#' ds.assign(opals, 'labhdl', 'D$LAB_HDL')
#' myobjects <- list('labtsc', 'labhdl')
#' ds.cbind(datasources=opals, objects=myobjects)
#' 
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
  
  if(length(objects) < 2){
    stop(" You must provide a list of at least to objects to combine!\n", call.=FALSE)
  }
  for(i in 1:length(objects)){
    typ <- dsbaseclient:::.checkClass(datasources, objects[[i]])
  }
  
  if(is.null(newobj)){
    newobj <- 'newObject'
  }
  
  # call the server side function
  cally <- call('cbindDS', objs=objects)
  datashield.assign(datasources, newobj, cally)
  
}