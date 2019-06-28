#' 
#' @title Gets the names of items in a list
#' @description This function is similar to the R function \code{names}.
#' @details In DataSHIELD the use of this function is restricted to objects of type list.
#' @param x a character, the name of an object of type list.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return The names of the list's elements for each study
#' @author Gaye, A.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load the login data
#'   data(logindata)
#' 
#'   # login and assign some variables to R
#'   myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # generates subset tables from the table assigned above (by default the table name is 'D')
#'   ds.subsetByClass(x='D', subsets='subclasses')
#' 
#'   # the above object 'subsets' is a list, let us display the names of elements in 'subsets'
#'   ds.names('subclasses')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.names <- function(x=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input list!", call.=FALSE)
  }else{
    defined <- isDefined(datasources, x)
  }
   
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a list
  if(typ != 'list'){
    stop("The input object must be a list.", call.=FALSE)
  }
  
  # call the server side function that does the job.
  cally <- paste0('namesDS(', x, ')')
  output <- opal::datashield.aggregate(datasources, as.symbol(cally))
  return(output)

}
