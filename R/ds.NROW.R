#' 
#' @title Returns the number of rows present in x
#' @description this function is similar to R function \code{NROW} with the addition that
#' it also generates a global number of rows (i.e.total number of entries across several datasets)
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a vector, array, matrix or data frame
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global number of rows is calculated 
#' if \code{type} is set to 'split', the number of rows is calculated separately for each study.
#' @return an \code{interger} of length 1 or \code{NULL}
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk) and Isaeva, J. (julia.isaeva@fhi.no)
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: Get the total number of observations across all the studies
#' ds.NROW(datasources=opals, x=quote(D))
#' 
#' # Example 2: Get the number of observations on each study
#' ds.NROW(datasources=opals, x=quote(D), type="split")
#' 
#' # Example 3: Get the total number of observations for the variable 'LAB_TSC' (should be the same as in Example 1)
#' ds.NROW(datasources=opals, x=quote(D$LAB_TSC))
#' 
#' }
#' 
ds.NROW = function(datasources=NULL, x=NULL, type='combine'){
  
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(x)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid object\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(x)
  datasources <- ds.checkvar(datasources, vars2check)
  
  cally <- call('NROW', x )
  numrows <- datashield.aggregate(datasources, cally)
  
  if(type=="combine"){
    pooled.numrows <- sum(unlist(numrows))
    return(list("total.number.of.observations"=round(pooled.numrows,4)))
  }else{
    if(type=="split"){
      return(numrows)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    } 
  }
  
}