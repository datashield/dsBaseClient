#' 
#' @title Provides access to the levels attribute of a factor variable
#' @description This function is similar to R function \code{levels}
#' @details this is just a wrapper function ofr the server side function.
#' @param xvect a character, the name of a factor variable
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
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
#' ds.levels(xvect='D$PM_BMI_CATEGORICAL')
#' 
#' # Example 2: Get the levels of the LAB_TSC   SHOULD NOT WORK AS IT IS A CONTINUOUS VARIABLE
#' \dontrun{ ds.levels(xvect='D$LAB_TSC') }
#' }
#' 
ds.levels = function(xvect=NULL, datasources=NULL) {
  
  # if no opal login details were provided look for 'opal' objects in the environment
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
  
  if(is.null(xvect)){
    message("\n ALERT!\n")
    message(" Please provide a valid factor vector.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources,xvect)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, xvect)
  
  # stop if input is not a factor
  if (typ!='factor'){
    stop(" The input vector must be a factor", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  xnames <- extract(xvect)
  varname <- xnames[length(xnames)]
  
  cally <- paste0("levels(", xvect, ")")
  levels_all <- datashield.aggregate(datasources, as.symbol(cally))
  
  return(levels_all)
}