#' 
#' @title Computes the exponential function
#' @description This function is similar to R function \code{exp}. 
#' @details this is a wrapper that calls the exponential function on the server site.
#' @param x a charcater, the name of a numerical vector.
#' @param newobj the name of the new vector.If this argument is set to \code{NULL}, the name of the new 
#' variable is the name of the input variable with the suffixe '_exp' (e.g. 'PM_BMI_CONTINUOUS_exp', if input 
#' variable's name is 'PM_BMI_CONTINUOUS')
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   myvar <- list("PM_BMI_CONTINUOUS")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # compute exponential function of the 'PM_BMI_CONTINUOUS' variable
#'   ds.exp(x='D$PM_BMI_CONTINUOUS')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.exp = function(x=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
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
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  if(!('numeric' %in% typ) && !('integer' %in% typ)){
      stop(" Only objects of type 'numeric' and 'integer' are allowed.", call.=FALSE)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_exp")
  }
  
  # call the server side function that does the job
  cally <- paste0('exp(', x, ')')
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
