#' 
#' @title Test for association between paired samples
#' @description This is similar to the R base function 'cor.test'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a character, the name of a numerical vector
#' @param yvect a character, the name of a numerical vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global mean is calculated 
#' if \code{type} is set to 'split', the mean is calculated separately for each study.
#' @return a list containing the results of the test
#' @author GAYE, A.; Burton, P.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list('LAB_TSC', 'LAB_HDL')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # test for correlation between the variables 'LAB_TSC' and 'LAB_HDL'
#' ds.corTest(datasources=opals, xvect='D$LAB_TSC', yvect='D$LAB_HDL')
#' }
#' 
ds.corTest = function(datasources=NULL, xvect=NULL, yvect=NULL, type='combine')
{
  
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message(" ALERT!")
    message(" Please provide a two valid numeric vectors")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(yvect)){
    message(" ALERT!")
    message(" Please provide a two valid numeric vectors")
    stop(" End of process!", call.=FALSE)
  }
  
  # name of the studies to be used in the output
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  cally <- paste0("cor.test(", xvect, ",", yvect, ")")
  res.local <- datashield.aggregate(datasources, as.symbol(cally))
  

  if (type=='split') {
    return(res.local)
  } else if (type=='combine') {
    return(res.local)
    
  } else{
    stop('Function argument "type" has to be either "combine" or "split"')
  }
  
}
