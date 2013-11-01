#' 
#' @title Gets the range of a vector
#' @description this function is similar to R function \code{range} but instead to not return 
#' the real minimum and maximum, the computed values are multiplied by a very small random number.
#' In addition it also generates a global range (i.e. the range of a vector across several datasets)
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global range is calculated 
#' if \code{type} is set to 'split', the range is calculated separately for each study.
#' @return a numeric vector which contains the minimum and the maximum
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
#' # Example 1: Get the global range of the variable 'LAB_TSC' - default behaviour
#' ds.range(datasources=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Example 2: Get the range of the variable 'LAB_TSC' for each study
#' ds.range(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
#' }
#' 
ds.range = function(datasources=NULL, xvect=NULL, type='combine'){
  
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
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  cally <- call('range.ds', xvect )
  ranges <- datashield.aggregate(datasources, cally)
  mins = unlist(lapply(ranges, function(x) x[1]))
  maxs = unlist(lapply(ranges, function(x) x[2]))
  
  if(type=="combine"){
    global.min = min(mins)
    global.max = max(maxs)
    global.range = c(global.min, global.max)
    return(global.range)
  }else{
    if(type=="split"){
      return(ranges)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    } 
  }
  
}