#' 
#' @title Provides access to the levels attribute of a factor variable
#' @description This function is similar to R function \code{levels}
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a character, the name of a factor variable
#' @return levels of xvect
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
#' # Example 1: Get the levels of the PM_BMI_CATEGORICAL variable
#' ds.levels(datasources=opals, xvect='D$PM_BMI_CATEGORICAL')
#' 
#' # Example 2: Get the levels of the LAB_TSC   SHOULD NOT WORK AS IT IS A CONTINUOUS VARIABLE
#' \dontrun{ ds.levels(datasources=opals, xvect='D$LAB_TSC') }
#' }
#' 
ds.levels = function(datasources=NULL, xvect=NULL) {
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message(" ALERT!")
    message(" Please provide a valid factor vector of type factor.")
    stop(" End of process!", call.=FALSE)
  }
  
  # check whether the vector is a factor type
  typ <- dsbaseclient:::.checkClass(datasources, xvect)
  
  if (typ!='factor'){
    stop(" The input vector is not a factor", call.=FALSE)
  }

  cally <- paste0("levels(", xvect, ")")
  levels_all <- datashield.aggregate(datasources, cally)
  
  return(levels_all)
}