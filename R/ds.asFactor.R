#' 
#' @title Turns a numeric vector into factor type
#' @description This function is similar to R function \code{as.factor} but it does not allow users
#' to create factors where a categorie has less than two observations.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a numerc vector
#' @param newobj the name of the new vector.If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_fact' (e.g. 'GENDER_fact', if input 
#' variable's name is 'GENDER')
#' @return a message is displayed when the action is completed.
#' @author Gaye, A.; Burton, P.
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
#' # turn the factor variable 'GENDER' into a character variable
#' ds.asCharacter(datasources=opals, xvect='D$GENDER', newobj='gender_char')
#' 
#' # turn the character variable 'gender_char' into a numeric variable
#' ds.asNumeric(datasources=opals, xvect='gender_char', newobj='gender_num')
#' 
#' # now turn the numeric variable 'gender_num' into a factor variable
#' ds.asFactor(datasources=opals, xvect='gender_num', newobj='gender_fact')
#' 
#' }
#' 
ds.asFactor = function(datasources=NULL, xvect=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n ALERT!\n")
    message(" Please provide a valid numeric vector.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  #vars2check <- list(xvect)
  #datasources <- ds.checkvar(datasources, vars2check)
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(xvect, "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    varname <- strsplit(xvect, "\\$", perl=TRUE)[[1]][2]
  }else{
    varname <- deparse(xvect)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_char")
  }
  
  # call the server side function that does the job
  cally <- paste0('asFactorDS(', xvect, ')' )
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # a message so the user know the function was ran (assign function are 'silent')
  #message("An 'assign' function was ran, no output should be expected on the client side!")
  
  # check that the new object has been created and display a message accordingly
  cally <- call('exists', newobj )
  qc <- datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc==TRUE))
  
  if(length(indx) > 0 & length(indx) < length(datasources)){
    warning("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!")
  }
  if(length(indx) == 0){
    warning("The output object has not been generated for any of the studies!")
  }
  
}
