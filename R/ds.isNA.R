#' 
#' @title Checks if a vector is empty 
#' @description this function is similar to R function \code{is.na} but instead of a vector 
#' of booleans it returns just one boolean to tell if all the elements are missing values.
#' @details In certain analyses such as GLM none of the variable should be missing at complete
#' (i.e. missing value for each observation). Since in DataSHIELD it is not possible to see the data
#' it is important to know whether or not a vector is empty to proceed accordingly.
#' @param x a character, the name of the vector to check.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter.
#' @return a boolean 'TRUE' if the vector is empty (all values are 'NA') and 'FALSE'  otherwise.
#' @author Gaye, A.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load the login data
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   myvar <- list("LAB_HDL")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#'   # check if all the observation of the variable 'LAB_HDL' are missing (NA)
#'   ds.isNA(x='D$LAB_HDL')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.isNA <- function(x=NULL, datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
    
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a vector
  if(!('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ) & !('numeric' %in% typ) & !('data.frame' %in% typ) & !('matrix' %in% typ)){
    stop("The input object must be a character, factor, integer, logical or numeric vector.", call.=FALSE)
  }
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # keep of the results of the checks for each study
  track <- list()
   
  # call server side function 'isNA.ds' to check, in each study, if the vector is empty
  for(i in 1: length(datasources)){
    cally <- paste0("isNaDS(", x, ")")
    out <- opal::datashield.aggregate(datasources[i], as.symbol(cally))
    if(out[[1]]){ 
      track[[i]] <- TRUE
      message("The variable ", varname, " in ", stdnames[i], " is missing at complete (all values are 'NA').")
    }else{
      track[[i]] <- FALSE
    }
  }
  names(track) <- stdnames
  return(track)
}
