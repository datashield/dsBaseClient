#' 
#' @title Computes the variance of a given vector (for several studies separately or combined)
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a string character, the name of a numerical vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global variance is calculated 
#' if \code{type} is set to 'split', the variance is calculated separately for each study.
#' @return a a global variance or one variance for each study.
#' @author Gaye, A.
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
#' # Example 1: compute the pooled variance of the variable 'LAB_TSC' - default behaviour
#' ds.var(datasources=opals, xvect='D$LAB_TSC')
#' 
#' # Example 2: compute the variance of each study separately
#' ds.var(datasources=opals, xvect='D$LAB_TSC', type='split')
#' }
#' 
ds.var = function(datasources=NULL, xvect=NULL, type='combine'){
  
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # name of the studies to be used in the output
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  cally <- paste0("varDS(", xvect, ")")
  var.local <- datashield.aggregate(datasources, as.symbol(cally))
  
  cally <- paste0("NROW(", xvect, ")")
  length.local <- datashield.aggregate(datasources, cally)
  
  if (type=='split') {
    return(var.local)
  } else if (type=='combine') {
    length.total = 0
    sum.weighted = 0
    var.global  = NA
    
    for (i in 1:num.sources){
      if ((!is.null(length.local[[i]])) & (length.local[[i]]!=0)) {
        length.total = length.total+length.local[[i]]
        sum.weighted = sum.weighted+length.local[[i]]*var.local[[i]]
      }
    }
    
    # get the pooled variance
    var.global = sum.weighted/length.total
    return(list("Global variance"=var.global))
    
  } else{
    stop('Function argument "type" has to be either "combine" or "split"')
  }
  
}
