#' 
#' @title Gets the length of a vector or list
#' @description This function is similar to R function \code{length}.
#' @details The function returns the pooled length or the length of the a vector or a list for 
#' each study.
#' @param x a string character, the name of a vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global variance is calculated 
#' if \code{type} is set to 'split', the variance is calculated separately for each study.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a numeric, the number of elements of the input vector or list.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#'   # load that contains the login details
#'   data(logindata)
#'   library(opal)
#'
#'   # login and assign all the variables stored on the server side
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: Get the total number of observations across all the studies for the
#'   # variable 'LAB_TSC' - default behaviour
#'   ds.length(x='D$LAB_TSC')
#' 
#'   # Example 2: Get the number of observations on each study, for the variable 'LAB_TSC'
#'   ds.length(x='D$LAB_TSC', type='split')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals) 
#' 
#' }
#' 
ds.length = function(x=NULL, type='combine', datasources=NULL){
  
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
  if(typ != 'character' & typ != 'factor' & typ != 'integer' & typ != 'logical' & typ != 'numeric' & typ != 'list'){
    stop("The input object must be a character, factor, integer, logical or numeric vector or a list.", call.=FALSE)
  }
  
  cally <- paste0("length(", x, ")")
  lengths <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  if(type=="combine"){
    pooled.length <- sum(unlist(lengths))
    return(list("total.number.of.observations"=round(pooled.length,4)))
  }else{
    if(type=="split"){
      return(lengths)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    } 
  }
  
}
