#' 
#' @title Retrieves the dimension of an object
#' @description this function is similar to R function \code{dim}
#' @details the function returns the polled dimension of the object by summing
#' up the individuals dimensions returned from each study. Unlike the other DataSHIELD
#' function the default behaviour is to output the dimension of each study separately. 
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a character, the name of R table object, for example a matrix, array or data frame
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global variance is calculated 
#' @return for an array, \code{NULL} or a vector of mode \code{integer}
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
#' # Example 1: Get the dimension of the assigned datasets in each study
#' ds.dim(datasources=opals, x='D')
#' 
#' # Example 2: Get the pooled dimension of the assigned datasets
#' ds.dim(datasources=opals, x='D', type='combine')
#' 
#' # Example 2: Input has to be either matrix, data frame or an array
#' \dontrun{ ds.dim(datasources=opals, x='D$LAB_TSC') }
#' }
#' 
ds.dim = function(datasources=NULL, x=NULL, type='split') {
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(x)){
    message(" ALERT!")
    message(" Please provide a valid matrix-like object")
    stop(" End of process!", call.=FALSE)
  }
  
  cally <- paste0("dim(", x, ")")
  dimensions <- datashield.aggregate(datasources, as.symbol(cally))
  
  if(type=="combine"){
    global.dim1 <- 0
    global.dim2 <- dimensions[[1]][2]
    for(i in 1:length(datasources)){
      global.dim1 <- global.dim1 + dimensions[[i]][1]
    }
    return(list("pooled.dimension"=c(global.dim1, global.dim2)))
  }else{
    if(type=="split"){
      return(dimensions)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    } 
  }
}