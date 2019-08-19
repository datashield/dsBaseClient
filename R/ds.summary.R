#' 
#' @title Generates the summary of an object
#' @description Provides some insight about an object. Unlike the similar R function
#' only a limited class of objects can be used as input to reduce the risk of disclosure.
#' @details The class and size of the object are returned and various other information are 
#' also returned depending of the class of the object. Potentially disclosive information
#' such as the minimum and maximum values of numeric vectors are not returned. The summary 
#' is given for each study separately.
#' @param x a numeric or factor variable
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @return the returned information depends on the class of the objects.
#' @author Gaye, A.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load the login data
#'   data(logindata)
#' 
#'   # login and assign all the variable held in the opal database
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: suummary of a numerical variable
#'   ds.summary(x='D$LAB_TSC')
#' 
#'   # Example 1: suummary of a binary variable
#'   ds.summary(x='D$GENDER')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.summary <- function(x=NULL, datasources=NULL){
  
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
  
  # call the internal function that checks if the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a numeric or an integer vector
  # the input object must be a dataframe or a factor
  if(!('data.frame' %in% typ) & !('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('list' %in% typ) & !('logical' %in% typ) & !('matrix' %in% typ) & !('numeric' %in% typ)){
    stop("The input object must be a 'data.frame', 'character', factor', 'integer', 'list', 'logical', 'matrix' or 'numeric'.", call.=FALSE)
  }
  
  stdnames <- names(datasources)
  numsources <- length(datasources)
  finalOutput <- list()
                                    
  # now get the summary depending on the type of the input variable
  if(("data.frame" %in% typ) | ("matrix" %in% typ)){
    for(i in 1:numsources){
      validity <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        dims <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('dim(', x, ')' )))
        r <- dims[[1]][1]
        c <- dims[[1]][2]
        cols <- (opal::datashield.aggregate(datasources[i], as.symbol(paste0('colnames(', x, ')' ))))[[1]]
        stdsummary <- list('class'=typ, 'number of rows'=r, 'number of columns'=c, 'variables held'=cols)
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }
  
  if("character" %in% typ){
    for(i in 1:numsources){
      validity <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('length(', x, ')' )))[[1]]
        stdsummary <- list('class'=typ, 'length'=l)
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }
  
  if("factor" %in% typ){
    for(i in 1:numsources){
      validity <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('length(', x, ')' )))[[1]]
        categories <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('levels(', x, ')' )))[[1]]
        freq <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('table1DDS(', x, ')' )))[[1]][1]
        stdsummary <- list('class'=typ, 'length'=l, 'categories'=categories)
        for(j in 1:length(categories)){
          stdsummary[[3+j]] <- freq[[1]][1,j]
        }
        names(stdsummary)[4:(3+length(categories))] <- paste0("count of '", categories, "'")
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }
  
  if(("integer" %in% typ) | ("numeric" %in% typ)){
    for(i in 1:numsources){
      validity <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('length(', x, ')' )))[[1]]
        q <- (opal::datashield.aggregate(datasources[i], as.symbol(paste0('quantileMeanDS(', x, ')' ))))[[1]]
        stdsummary <- list('class'=typ, 'length'=l, 'quantiles & mean'=q)
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    
    }
    names(finalOutput) <- stdnames
  }
  
  if("list" %in% typ){
    for(i in 1:numsources){
      l <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('length(', x, ')' )))[[1]]
      elts <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('namesDS(', x, ')' )))[[1]]
      if(is.null(elts)){
        stdsummary <- list('class'=typ, 'length'=l)
      }else{
        stdsummary <- list('class'=typ, 'length'=l, 'elements held in the list'=elts)
      }
      finalOutput[[i]] <- stdsummary
    }
    names(finalOutput) <- stdnames
  }
  
  if("logical" %in% typ){
    for(i in 1:numsources){
      validity <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('length(', x, ')' )))[[1]]
        freq <- opal::datashield.aggregate(datasources[i], as.symbol(paste0('table1DDS(', x, ')' )))[[1]][1]
        stdsummary <- list('class'=typ, 'length'=l)
        for(j in 1:length(2)){
          stdsummary[[2+j]] <- freq[[1]][1,j]
        }
        names(stdsummary)[3:(2+2)] <- paste0("count of '", c('FALSE','TRUE'), "'")
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }
  
  return(finalOutput)
}
