#' 
#' @title Computes the variance of a given vector (for several studies separately or combined)
#' @description This function is similar to the R function \code{var}.
#' @details It is a wrapper for the server side function
#' @param x a character, the name of a numerical vector.
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global variance is calculated 
#' if \code{type} is set to 'split', the variance is calculated separately for each study.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
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
#' ds.var(x='D$LAB_TSC')
#' 
#' # Example 2: compute the variance of each study separately
#' ds.var(x='D$LAB_TSC', type='split')
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#' }
#' 
ds.var = function(x=NULL, type='combine', datasources=NULL){
  
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
        userInput <- readline("Please enter the name of the login object you want to use: ")
        datasources <- eval(parse(text=userInput))
        if(class(datasources[[1]]) != 'opal'){
          stop("End of process: you failed to enter a valid login object", call.=FALSE)
        }
      }
    }
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
  
  # the input object must be a numeric or an integer vector
  if(typ != 'integer' & typ != 'numeric'){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }
  
  
  # number of studies
  num.sources <- length(datasources)
  
  cally <- paste0("varDS(", x, ")")
  var.local <- datashield.aggregate(datasources, as.symbol(cally))
  
  cally <- paste0("NROW(", x, ")")
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
