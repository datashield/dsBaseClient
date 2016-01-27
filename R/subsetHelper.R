#' 
#' @title Compares subset and original object sizes and eventually carries out subsetting
#' @description This is an internal function.
#' @details This function is called by the function 'ds.subset' to ensure that the requested subset
#' is not larger than the original object.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param data a string character, the name of the dataframe or the factor vector and the range of the subset.
#' @param rs a vector of two integers, the indices of the rows de extract. 
#' @param cs a vector of two integers or one or more characters.
#' @keywords internal
#' @return a message or the class of the object if the object has the same class in all studies.
#'
subsetHelper <- function(dts, data, rs=NULL, cs=NULL){

  # if the size of the requested subset is greater than that of original inform the user and stop the process
  dims <- opal::datashield.aggregate(dts, paste0("dim(", data, ")"))
  fail <- c(0,0)
  
  if(!(is.null(rs))){
    if(length(rs) > dims[[1]][1] ){ 
      fail[1] <- 1 
    } 
  }
  
  if(!(is.null(cs))){
    if(length(cs) > dims[[1]][2]){ 
      fail[2] <- 1 
    }
  }
  return(fail)
  
}