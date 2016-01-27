#' 
#' @title Turns a vector into numerical type
#' @description This function is similar to R function \code{as.numeric}.
#' @details See details of the R function 'as.numeric'.
#' @param x a character, the name of the input vector.
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_num'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @seealso \link{ds.asFactor} to turn a variable into a factor type.
#' @seealso \link{ds.asList} to contruct an object of type list.
#' @seealso \link{ds.asCharacter} to turn a variable into a character type.
#' @seealso \link{ds.asMatrix} to coerce an object into a matrix type.
#' @export
#' @examples {
#' 
#'   # load that contains the login details
#'   data(logindata)
#'   library(opal)
#'
#'   # login and assign specific variable(s)
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   myvar <- list("GENDER")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # turn the factor variable 'GENDER' into a character vector
#'   ds.asCharacter(x='D$GENDER', newobj="gender_ch")
#' 
#'   # now turn the newly created vector 'gender_ch' into a numeric
#'   ds.asNumeric(x='gender_ch', newobj="gender_nm")
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.asNumeric = function(x=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_num")
  }
  
  # call the server side function that does the job
  cally <- paste0('as.numeric(as.character(', x, '))' )
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
