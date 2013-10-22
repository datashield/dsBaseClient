#' 
#' @title Turns a vector into numerical type
#' @description This function is similar to R function \code{as.numeric}. It is important 
#' to NOTE THAT FACTOR SHOULD BE TURNED INTO 'character' first AND THEN ONLY INTO 'numeric'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a vector.
#' @param newvect the name of the new variable. If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_num' (e.g. 'GENDER_num', if input 
#' variable's name is 'GENDER)
#' @return a message is displayed when the action is completed.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # turn the factor variable 'GENDER' into a character vector
#' ds.asCharacter(datasources=opals, xvect=quote(D$GENDER), newvect="gender_ch")
#' 
#' # now turn the newly created vector 'gender_ch' into a numeric
#' ds.asNumeric(datasources=opals, xvect=quote(D$GENDER), newvect="gender_nm")
#' }
#' 
ds.asNumeric = function(datasources=NULL, xvect=NULL, newvect=NULL){
  
  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    varname <- strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    varname <- deparse(xvect)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newvect)){
    newvect <- paste0(varname, "_num")
  }
  
  # call the server side function that does the job
  cally <- call('as.numeric', xvect )
  datashield.assign(datasources, newvect, cally)
  
  # a message so the user know the function was ran (assign function are 'silent')
  message("\nAn assign function was ran, no output should be expected on the client side!\n\n")
}
