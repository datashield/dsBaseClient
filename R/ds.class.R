#' 
#' @title Retrieves the class of an object
#' @description This function is similar to R function \code{class}.
#' @details Same as for the R function \code{class}.
#' @param x an R object
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a character the type of x
#' @author Gaye, A.; Isaeva, J.
#' @seealso \link{ds.exists} to verify if an object is defined (exists) on the server side.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign all the stored variables
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: Get the class of the whole dataset
#'   ds.class(x='D')
#' 
#'   # Example 2: Get the class of the variable PM_BMI_CONTINUOUS
#'   ds.class(x='D$LAB_TSC')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.class <- function(x=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  cally <- paste0('class(', x, ')')
  output <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  return(output)

}
