#' 
#' @title Computes the variance of a given vector (for several studies separately or combined)
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a numerical vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global variance is calculated 
#' if \code{type} is set to 'split', the variance is calculated separately for each study.
#' @return a mean value
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
#' # Example 1: compute the pooled variance of the variable 'LAB_TSC' - default behaviour
#' ds.var(datasources=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Example 2: compute the variance of each study separately
#' ds.var(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
#' }
#' 
ds.var = function(datasources=NULL, xvect=NULL, type='combine'){
  
  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # name of the studies to be used in the output
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  cally <- call('var.ds', xvect )
  variances <- datashield.aggregate(datasources, cally)
  
  if(type=="combine"){
    pooled.variance <- sum(unlist(variances))/numsources
    return(list("pooled.variance"=round(pooled.variance,4)))
  }else{
    if(type=="split"){
      return(variances)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    } 
  }
  
}
