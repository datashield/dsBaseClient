#' 
#' @title Tells user if an object is defined on the server side.
#' @description Because server side objects are not visible to users, tis function gives some information.
#' @details the function checks if the specified object is defined (i.e. exists) on the server side and prints
#' a message to inform the user.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param obj a string charcater, the name of the object to check for.
#' @return a message
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load table that contains the login details
#' data(logindata)
#'
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC", "LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # get the log values of the variable 'LAB_TSC' in D (default name of the assigned table) and assign it to 'logTSC'
#' ds.assign(datasources=opals, newobj="logTSC", toAssign="log(D$LAB_TSC)")
#' 
#' # now check if the object you just assigned is really stored on the server side
#' ds.inform(datasources=opals, obj="logTSC")
#' 
#' }
#' 
ds.inform <- function(datasources=NULL, obj=NULL){
  
  if(is.null(datasources)){
    message("ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!\n\n", call.=FALSE)
  }else{
    stdnames <- names(datasources)
    if(is.null(obj)){
      message("Please provide a valid object to assign!")
      stop("End of process!\n", call.=FALSE)
    }
  }

  qc <- ds.exists(datasources, obj)
  indx <- as.numeric(which(qc==TRUE))
  if(length(indx) == length(datasources)){
    message("The object '", obj, "' is stored on the server side.")
  }else{
    if(length(indx) > 0){
      warning(paste0("The object '", obj, "' is defined only in ", paste(stdnames[indx],collapse=", "), "!"), call.=FALSE)
    }
    if(length(indx) == 0){
      warning(paste0("The object '", obj, "' is not defined in any of the studies!"), call.=FALSE)
    }
  }

}