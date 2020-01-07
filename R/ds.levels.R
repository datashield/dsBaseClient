#' 
#' @title Returns the levels attribute of a factor
#' @description This function is similar to R function \code{levels}
#' @details This is just a wrapper function for the server side function.
#' @param  x a character, the name of a factor variable
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return levels of x
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: Get the levels of the PM_BMI_CATEGORICAL variable
#'   ds.levels(x='D$PM_BMI_CATEGORICAL')
#' 
#'   # Example 2: Get the levels of the LAB_TSC   SHOULD NOT WORK AS IT IS A CONTINUOUS VARIABLE
#'   ds.levels(x='D$LAB_TSC')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.levels = function(x=NULL, datasources=NULL) {
  
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
  
  # the input object must be a factor
  if(typ != 'factor'){
    stop("The input object must be a factor.", call.=FALSE)
  }  
  
  cally <- paste0("levels(", x, ")")
  levels_all <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  return(levels_all)
}
