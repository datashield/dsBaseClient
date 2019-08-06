#' 
#' @title Checks if an object is valid
#' @description Checks if a vector or table structure has a number of observations equal to or greater
#' than the threshold set by DataSHIELD.
#' @details In DataSHIELD, analyses are possible only on valid objects to ensure the ouput is not disclosive.
#' This function checks if an input object is valid. A vector is valid if the 
#' number of observations are equal to or greater than a set threshold. A factor vector is valid if all
#' its levels (categories) have a count equal or greater than the set threshold. A dataframe or a matrix 
#' is valid if the number of rows is equal or greater than the set threshold. 
#' @param x a character, the name of a vector, dataframe or matrix.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter.
#' @return  a boolean, TRUE if input object is valid and FALSE otherwise.
#' @author Gaye, A.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   myvar <- list("LAB_TSC", "GENDER")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # Example 1: Check if the dataframe assigned above is valid
#'   ds.isValid(x='D')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.isValid = function(x=NULL, datasources=NULL){
  
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
  
  # the input object must be a vector
  if(!('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ) & !('numeric' %in% typ) & !('data.frame' %in% typ) & !('matrix' %in% typ)){
    stop("The input object must be a character, factor, integer, logical or numeric vector or a dataframe or a matrix", call.=FALSE)
  }
  
  # call the server side function that does the job and return its output
  cally <- paste0('isValidDS(', x, ')')
  output <- opal::datashield.aggregate(datasources, as.symbol(cally))
  return(output)
}
