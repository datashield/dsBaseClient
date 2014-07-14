#' 
#' @title Assigns an object to a name in the server side
#' @description This function assigns a datashield object to a name, hence creating a new object. 
#' It also calls 'assign' server side functions to generate objects stored on the server side.
#' The function is a wrapper for the 'opal' package function 'datashield.assign'.
#' @details The new object is stored on the remote R instance (i.e. on the server side).
#' If no name is provided, the new object is named 'newObject', by default.
#' @param toAssign a string character, the object to assign or the call to an assign function 
#' that generates the object to assign.
#' @param newobj the name of the new object
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
#' # (by default the assigned dataset is a dataframe named 'D')
#' myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: assign the variable 'LAB_TSC' in the dataframe D
#' ds.assign(toAssign='D$LAB_TSC', newobj='labtsc')
#' 
#' # Example2: get the log values of the variable 'LAB_TSC' in D and assign it to 'logTSC'
#' ds.assign(toAssign='log(D$LAB_TSC)', newobj='logTSC')
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#' }
#' 
ds.assign <- function(toAssign=NULL, newobj="newObject", datasources=NULL){
  
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        userInput <- readline("Please enter the name of the login object you want to use: ")
        datasources <- eval(parse(text=userInput))
        if(class(datasources[[1]]) != 'opal'){
          stop("End of process: you failed to enter a valid login object", call.=FALSE)
        }
      }
    }
  }
  
  if(is.null(toAssign)){
    stop("Please the name of object to assign or an expression to evaluate and assign.!\n", call.=FALSE)
  }
  
  # now do the business
  datashield.assign(datasources, newobj, as.symbol(toAssign))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
