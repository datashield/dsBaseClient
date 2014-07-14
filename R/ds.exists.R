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
#' ds.assign(toAssign='D$LAB_TSC', newobj='labtsc')
#'
#' # now let us check if the variable 'labtsc' does now 'exist' on the server side
#' ds.exists(x='labtsc')
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#' }
#' 
ds.exists <- function(x=NULL, datasources=NULL){
  
  # if no opal login details were provided look for 'opal' objects in the environment
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
  
  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  # call the server side function that does the job
  cally <- call("exists", x)
  output <- datashield.aggregate(datasources, cally)
  
  return(output)
}
