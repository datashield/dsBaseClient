#' 
#' @title Computes logarithms, by default natural logarithms
#' @description This function is similar to R function \code{log}.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a vector.
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_log' (e.g. 'LAB_TSC_log', if input 
#' variable's name is 'LAB_TSC')
#' @return a message is displayed when the action is completed.
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk) and Isaeva, J. (julia.isaeva@fhi.no)
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
#' # Compute natural logarithm of LAB_TSC
#' ds.log(datasources=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Compute natural logarithm of LAB_TSC with base 2
#' ds.log(datasources=opals, xvect=quote(D$LAB_TSC), base=2)
#' 
#' }
#' 
ds.log = function(datasources=NULL, xvect=NULL, base=exp(1), newobj=NULL){
  
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  #   # call the function that checks the variable is available and not empty
  #   vars2check <- list(xvect)
  #   datasources <- ds.checkvar(datasources, vars2check)
  
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
  if(is.null(newobj)){
    newobj <- paste0(varname, "_log")
  }
  
  # call the server side function that does the job
  cally <- call('log', xvect, base )
  datashield.assign(datasources, newobj, cally)
  
  # a message so the user know the function was run (assign function are 'silent')
  message("An 'assign' function was run, no output should be expected on the client side!")
  
  # check that the new object has been created and display a message accordingly
  cally <- call('exists', newobj )
  qc <- datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc==TRUE))
  if(length(indx) == length(datasources)){
    message("The output of the function, '", newobj, "', is stored on the server side.")
  }else{
    if(length(indx) > 0){
      warning("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!")
    }
    if(length(indx) == 0){
      warning("The output object has not been generated for any of the studies!")
    }
  }
  
}