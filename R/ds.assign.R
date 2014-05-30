#' 
#' @title Assigns an object to a name in the server side
#' @description This function assigns a datashield object to a name, hence creating a new object. 
#' It also calls 'assign' server side functions to generate objects stored on the server side.
#' The function is a wrapper for the 'opal' package function 'datashield.assign'.
#' @details The new object is stored on the local R instance (i.e. on the server side).
#' If no name is provided, the new object is named 'newObject', by default.
#' @param newobj the name of the new object
#' @param toAssign a string character, the object to assign or the call to an assign function 
#' that generates the object to assign.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: assign the variable 'LAB_TSC' in the dataframe D
#' ds.assign(newobj='labtsc', toAssign='D$LAB_TSC')
#' 
#' # Example2: get the log values of the variable 'LAB_TSC' in D and assign it to 'logTSC'
#' ds.assign(newobj='logTSC', toAssign='log(D$LAB_TSC)')
#' }
#' 
ds.assign <- function(newobj="newObject", toAssign=NULL, datasources=NULL){
  
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        stop(" Please set the parameter 'datasources' to the list you want to use. ", call.=FALSE)
      }
    }
  }
  
  if(is.null(toAssign)){
    message("\n ALERT!\n")
    message(" No object to assign or expression to evalualate and assign.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # now do the business
  datashield.assign(opals, newobj, as.symbol(toAssign))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}