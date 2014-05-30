#' 
#' @title Retrieves the class of an object
#' @description This function is similar to R function \code{class}
#' @param x an R object
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a character the type of x
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: Get the class of the whole dataset
#' ds.class(x='D')
#' 
#' # Example 2: Get the class of the variable PM_BMI_CONTINUOUS
#' ds.class(x='D$LAB_TSC')
#' }
#' 
ds.class = function(x=NULL, datasources=NULL) {
  
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        stop(" Please set the parameter 'datasources' to the list you want to use. ", call.=FALSE)
      }
    }
  }
  
  if(is.null(x)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid object\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  cally <- paste0('class(', x, ')')
  output <- datashield.aggregate(datasources, as.symbol(cally))
  
  return(output)
}