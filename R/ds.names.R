#' 
#' @title Gets the names of items in a list
#' @details This function is similar to the R function \code{names} but in DataSHIELD its use
#' is restricted to objects of type list.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xlist a string character, the name of an object of type list
#' @return  a list which hold items names for each study
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load the login data
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generates subset tables from the table assigned above (by default the table name is 'D')
#' ds.subclass(datasources=opals, subsets='subclasses', data='D')
#' 
#' # the above object 'subsets' is a list, let us display the names of elements in 'subsets'
#' ds.names(opals, 'subclasses')
#' 
#' }
#' 
ds.names = function(datasources=NULL, xlist=NULL){
  
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(xlist)){
    message(" ALERT!")
    message(" Please provide a valid numeric vector")
    stop(" End of process!", call.=FALSE)
  }
  
  # check if the input object is a list, call 
  cally <- paste0("is.list(", xlist, ")")
  check <- datashield.aggregate(datasources, as.symbol(cally))
  numsources <- length(datasources)
  stdnames <- names(datasources)
  if(sum(unlist(check)) == numsources){
    # call the server side function that does the job and return its output
    cally <- paste0("namesDS(", xlist, ")")
    output <- datashield.aggregate(datasources, as.symbol(cally))
    return(output)
  }else{
    indx <- which(unlist(check) == FALSE)
    if(length(indx) == numsources){
      message("\nThe input object is not of type list in any of the ", numsources, " studies\n")
      return(NULL)
    }else{
      message("\nThe input object is not of type list in ", paste0(stdnames[indx], collapse=", "),"\n")
      cally <- call('namesDS', xlist)
      output <- datashield.aggregate(datasources[-indx], cally)
      return(output)
    }
  }
  
}