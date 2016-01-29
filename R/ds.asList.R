#' 
#' @title Constructs an object of type list
#' @description This function is similar to R function \code{as.list} with some restrictions. 
#' @details Unlike the R function 'as.list' only certain object types (data.frame, matrix) can be turned 
#' into a list, this is because turning a single vector into a list produces a list where each element 
#' holds one value only. A matrix is turned into data.frame before being converted into a list.
#' When a data.frame, matrix is turned into a list the output list is allowed only if the number of rows 
#' of the input data.frame or matrix is greater than the allowed number of observations. Otherwise 
#' a list missing at complete is generated (i.e. all elements set to NA)).
#' @param x a character, the name of the object to convert into a list
#' @param newobj the name of the new vector.If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_list'
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @seealso \link{ds.asNumeric} to turn a variable into a numeric type.
#' @seealso \link{ds.asFactor} to turn a variable into a factor type.
#' @seealso \link{ds.asCharacter} to turn a variable into a character type.
#' @seealso \link{ds.asMatrix} to coerce an object into a matrix type.
#' @export
#' @examples {
#' 
#'   # load that contains the login details
#'   data(logindata)
#'   library(opal)
#'
#'   # login and assign all the stored variable(s)
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # turn the dataframe 'D' (the default name of the dataframe assign above) into a list
#'   ds.asList(x='D')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.asList = function(x=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # Only a dataframe or a matrice can be turned into a list
  if(typ != 'data.frame' & typ != 'matrix'){
    stop("Only objects of type 'data.frame' or 'matrix' are allowed. Please see documentation.", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_list")
  }
  
  # call the server side function that does the job
  cally <- paste0("asListDS(", x, ")")
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}