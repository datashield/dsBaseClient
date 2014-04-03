#' 
#' @title Computes the statistical mean of a given vector (for several studies separately or combined)
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a character, the name of a numerical vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global mean is calculated 
#' if \code{type} is set to 'split', the mean is calculated separately for each study.
#' @return a mean value
#' @author Isaeva, J.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list('LAB_TSC')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: compute the pooled statistical mean of the variable 'LAB_TSC' - default behaviour
#' ds.mean(datasources=opals, xvect='D$LAB_TSC')
#' 
#' # Example 2: compute the statistical mean of each study separately
#' ds.mean(datasources=opals, xvect='D$LAB_TSC', type='split')
#' }
#' 
ds.mean = function(datasources=NULL, xvect=NULL, type='combine')
{
  
  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # name of the studies to be used in the output
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  cally <- paste0("meanDS(", xvect, ")")
  mean.local <- datashield.aggregate(datasources, as.symbol(cally))
  
  cally <- paste0("NROW(", xvect, ")")
  length.local <- datashield.aggregate(datasources, cally)
  
  if (type=='split') {
    return(mean.local)
  } else if (type=='combine') {
    length.total = 0
    sum.weighted = 0
    mean.global  = NA
    
    for (i in 1:num.sources){
      if ((!is.null(length.local[[i]])) & (length.local[[i]]!=0)) {
        length.total = length.total+length.local[[i]]
        sum.weighted = sum.weighted+length.local[[i]]*mean.local[[i]]
      }
    }
    
    mean.global = sum.weighted/length.total
    
    if (!is.na(mean.global))
      return(list("Global mean"=mean.global))
    
  } else{
    stop('Function argument "type" has to be either "combine" or "split"')
  }
  
}
