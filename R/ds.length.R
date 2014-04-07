
#' 
#' @title Gets the length of a vector
#' @description this function is similar to R function \code{length} with the addition that
#' it also generates a global length (i.e. of the lengths of a vector across several datasets)
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a string character, the name of a vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global variance is calculated 
#' if \code{type} is set to 'split', the variance is calculated separately for each study.
#' @return a numerical value, the number of elements of the input vector
#' @author Gaye, A.
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
#' # Example 1: Get the total number of observations across all the studies for the variable 'LAB_TSC' - default behaviour
#' ds.length(datasources=opals, xvect='D$LAB_TSC')
#' 
#' # Example 2: Get the number of observations on each study, for the variable 'LAB_TSC'
#' ds.length(datasources=opals, xvect='D$LAB_TSC', type='split')
#' }
#' 
ds.length = function(datasources=NULL, xvect=NULL, type='combine'){
  
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
  #vars2check <- list(xvect)
  #datasources <- ds.checkvar(datasources, vars2check)
  
  cally <- paste0("length(", xvect, ")")
  lengths <- datashield.aggregate(datasources, as.symbol(cally))
  
  if(type=="combine"){
    pooled.length <- sum(unlist(lengths))
    return(list("total.number.of.observations"=round(pooled.length,4)))
  }else{
    if(type=="split"){
      return(lengths)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    } 
  }
  
}
