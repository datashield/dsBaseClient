#' 
#' @title Gets the names of items in a list
#' @details This function is similar to the R function \code{names} but in DataSHIELD its use
#' is restricted to objects of type list.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xlist an object of type list
#' @return  a list which hold items names for each study
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk)
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
#' datashield.assign(opals, "Subsets", quote(subsetdata.ds(D)))
#' 
#' # the above object 'Subsets' is a list, let us display the names of elements in 'Subsets'
#' ds.names(opals, quote(Subsets))
#' 
#' }
#' 
ds.names = function(datasources=NULL, xlist=NULL){
  
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xlist)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # check if the input object is a list, call 
  cally <- call("is.list", xlist)
  check <- datashield.aggregate(datasources, cally)
  numsources <- length(datasources)
  stdnames <- names(datasources)
  if(sum(unlist(check)) == numsources){
    # call the server side function that does the job and return its output
    cally <- call('names.ds', xlist)
    output <- datashield.aggregate(datasources, cally)
    return(output)
  }else{
    indx <- which(unlist(check) == FALSE)
    if(length(indx) == numsources){
      message("\nThe input object is not of type list in any of the ", numsources, " studies\n")
      return(NULL)
    }else{
      message("\nThe input object is not of type list in ", paste0(stdnames[indx], collapse=", "),"\n")
      cally <- call('names.ds', xlist)
      output <- datashield.aggregate(datasources[-indx], cally)
      return(output)
    }
  }
  
}