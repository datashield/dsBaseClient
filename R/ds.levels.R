#' 
#' @title Provides access to the levels attribute of a variable
#' @description this function is similar to R function \code{levels}
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect an R object (factor)
#' @return levels of xvect
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk) and Isaeva, J. (julia.isaeva@fhi.no)
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
#' ds.levels(datasources=opals, xvect=quote(D$PM_BMI_CATEGORICAL))
#' 
#' # Example 2: Get the levels of the LAB_TSC   SHOULD NOT WORK AS IT IS A CONTINUOUS VARIABLE
#' ds.levels(datasources=opals, xvect=quote(D$LAB_TSC))
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
    message("\n\n ALERT!\n")
    message(" Please provide a valid factor vector of type factor\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # check whether a given vector is a factor type
  numsources = length(datasources)
  cally = call('class', xvect)
  classes_xvect = datashield.aggregate(datasources, cally)
  flag = 0
  for (i in 1:numsources) {
    if (classes_xvect[[i]]!='factor') {
      message("\n\n ALERT!\n")
      message(" Please provide a valid factor vector of type factor for study ", i, "\n")
      flag=1
    }
  }
  if (flag==1)
    stop(" End of process!\n\n", call.=FALSE)
  
  #   if(!is.factor(xvect)){
  #     message("\n\n ALERT!\n")
  #     message(" Please provide a valid factor vector of type factor\n")
  #     stop(" End of process!\n\n", call.=FALSE)
  #   }
  #   
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  cally <- call('levels', xvect )
  levels_all <- datashield.aggregate(datasources, cally)
  
  return(levels_all)
}