#' 
#' @title Checks if the objecs are defined in studies
#' @description This is an internal function.
#' @details In DataSHIELD an object included in analysis must be defined (i.e. exists)
#' in all the studies. If not the process should halt.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param obj a character vector, the name of the objects to look for.
#' @keywords internal
#' @return a boolean vector.
#'
isDefined <- function(datasources=NULL, obj=NULL){
  
  stdnames <- names(datasources)

  inputnames <- c()
  inputobj <- unlist(obj)
  for(i in 1:length(inputobj )){
    chnames <- extract(inputobj[i])
    if(is.na(chnames[[1]])){
      inputnames <- append(inputnames, chnames[[2]])
    }else{
      inputnames <- append(inputnames, chnames[[1]])
    }
  }
  
  myObjects <- inputnames
  results <- c()
  for(i in 1:length(myObjects)){
    cally <- call('exists', myObjects[i])
    x <- opal::datashield.aggregate(datasources, cally)
    results <- append(results, mean(unlist(x)))
  }
  if(mean(results) != 1){
    idx <- which(results == FALSE)
    stop("The input object(s) ", paste(myObjects[idx],collapse=", ")," is(are) not defined on one or more of the studies!", call.=FALSE)
  }else{
    return(TRUE)
  }

}
