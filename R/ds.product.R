#' 
#' @title Computes a product
#' @details The function computes the product of up to five elements
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xlist a list of numerical to compute a product for.
#' @param newobj the name of the product object. If this argument is set to NULL, the name of the new 
#' object is 'prodOutput'.
#' variable's name is 'GENDER)
#' @return a numerical
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk)
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
#' # compute the product of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
#' prodinput <- list(quote(D$LAB_TSC), quote(D$LAB_HDL))
#' ds.product(datasources=opals, xlist=prodinput, newobj=NULL)
#' }
#' 
ds.product = function(datasources=NULL, xlist=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(xlist)){
    message("\n ALERT!\n")
    message(" Please provide a valid list of element to compute a product for.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- xlist
  datasources <- ds.checkvar(datasources, vars2check)
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "prodOutput"
  }
  
  # call the server side function that does the job
  cally <- call('product.ds', xlist)
  datashield.assign(datasources, newobj, cally)
  
  # a message so the user know the function was ran (assign function are 'silent')
  message("An 'assign' function was ran, no output should be expected on the client side!")
  
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
